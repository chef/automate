package server

import (
	"context"
	"fmt"
	"log"
	"os"
	"os/exec"
	"strings"

	"github.com/chef/automate/api/config/deployment"
	config "github.com/chef/automate/api/config/shared"
	api "github.com/chef/automate/api/interservice/deployment"
	"github.com/chef/automate/components/automate-deployment/pkg/converge"
	"github.com/chef/automate/components/automate-deployment/pkg/events"
	"github.com/imdario/mergo"
	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"
	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/status"
)

const (
	rsyslogConfigFile   = "/etc/rsyslog.d/automate.conf"
	logRotateConfigFile = "/etc/logrotate.d/automate"
)

// GetAutomateConfig returns a copy of the userOverrideConfig of the existing
// deployment.
func (s *server) GetAutomateConfig(ctx context.Context,
	req *api.GetAutomateConfigRequest) (*api.GetAutomateConfigResponse, error) {

	// If they haven't configured a deployment we can't get config
	if !s.HasConfiguredDeployment() {
		return nil, ErrorNotConfigured
	}

	s.deployment.Lock()
	defer s.deployment.Unlock()

	// Create a copy of our persisted config
	copy, err := s.deployment.GetUserOverrideConfigForPersistence().NewDeepCopy()
	if err != nil {
		return nil, status.Error(codes.FailedPrecondition, "Unable to acquire existing configuration")
	}

	hash, err := copy.Sum64()
	if err != nil {
		return nil, status.Error(codes.FailedPrecondition, "Unable to determine existing configuration hash")
	}

	return &api.GetAutomateConfigResponse{
		Config: copy,
		Hash:   hash,
	}, nil
}

// PatchAutomateConfig takes a PatchAutomateConfigRequest and merges it
// the existing userOverrideConfig and applies it to the existing deployment.
// The client must submit the hash of the config that was used on the client
// side to generate the new override config in order to verify that what we
// persist is the users config merged with the existing config.
func (s *server) PatchAutomateConfig(ctx context.Context,
	req *api.PatchAutomateConfigRequest) (*api.PatchAutomateConfigResponse, error) {

	// If they haven't configured a deployment we can't patch config
	if !s.HasConfiguredDeployment() {
		return nil, ErrorNotConfigured
	}

	operation := func(s *server) error {
		// Use a copy to make sure none of our operations modifies the
		// userOverrideConfig until we're sure it should change.

		existingCopy, err := s.deployment.GetUserOverrideConfigForPersistence().NewDeepCopy()
		if err != nil {
			return status.Error(codes.FailedPrecondition, "Unable to acquire existing configuration")
		}

		existingHash, err := existingCopy.Sum64()
		if err != nil {
			return status.Error(codes.FailedPrecondition, "Unable to determine existing configuration hash")
		}

		// Compare our existing configurations hash with the hash of the configuration
		// that was returned to the client when the request was initiated. If
		// they don't match then the configuration changed before they submitted
		// the configuration update.
		if existingHash != req.Hash {
			return status.Error(codes.DeadlineExceeded, "The configuration has changed since you initiated the update request. Please try again.")
		}

		if err = req.GetConfig().GetGlobal().ValidateReDirectSysLogConfig(); err != nil {
			return status.Error(codes.InvalidArgument, err.Error())
		}
		if err = setConfigForRedirectLogs(req, existingCopy); err != nil {
			logrus.Errorf("Unable to set config for redirect logs with error %v", err)
			return status.Error(codes.Internal, "Failed to set configuration for redirecting logs for automate")
		}

		if err = existingCopy.OverrideConfigValues(req.Config); err != nil {
			return status.Error(codes.Internal, "Failed to merge configuration into existing configuration")
		}

		if err = existingCopy.ValidateWithGlobalAndDefaults(); err != nil {
			return status.Error(codes.InvalidArgument, err.Error())
		}

		if err = s.deployment.ReplaceUserOverrideConfig(existingCopy, s.secretStore); err != nil {
			return status.Error(codes.Internal, err.Error())
		}

		if err = s.updateExpectedServices(); err != nil {
			return status.Error(codes.Internal, err.Error())
		}

		if err = s.persistDeployment(); err != nil {
			return status.Error(codes.Internal, err.Error())
		}

		return nil
	}

	// Kick off a new converge and pass our task back so that the client can
	// follow the event stream.
	sender := s.newEventSender()
	errHandler := deployErrorHandler(sender)
	sink := newConfigEventSink(sender)
	task, err := s.doConverge(operation, sender, sink, errHandler)
	if err != nil {
		return nil, err
	}

	return &api.PatchAutomateConfigResponse{TaskId: task.ID.String()}, nil
}

func (s *server) SetAutomateConfig(ctx context.Context,
	req *api.SetAutomateConfigRequest) (*api.SetAutomateConfigResponse, error) {

	// If they haven't configured a deployment we can't set config
	if !s.HasConfiguredDeployment() {
		return nil, ErrorNotConfigured
	}

	// Lock the deployment while we merge the config and persist it
	operation := func(s *server) error {
		var err error

		if err = req.Config.ValidateWithGlobalAndDefaults(); err != nil {
			return status.Error(codes.InvalidArgument, err.Error())
		}

		if err = s.deployment.ReplaceUserOverrideConfig(req.Config, s.secretStore); err != nil {
			return status.Error(codes.Internal, err.Error())
		}

		if err = s.updateExpectedServices(); err != nil {
			return status.Error(codes.Internal, err.Error())
		}

		if err = s.persistDeployment(); err != nil {
			return status.Error(codes.Internal, err.Error())
		}

		return nil
	}

	// Kick off a new converge and pass our task back so that the client can
	// follow the event stream.
	sender := s.newEventSender()
	errHandler := deployErrorHandler(sender)
	sink := newConfigEventSink(sender)
	task, err := s.doConverge(operation, sender, sink, errHandler)
	if err != nil {
		return nil, err
	}

	return &api.SetAutomateConfigResponse{TaskId: task.ID.String()}, nil
}

// configEventSink implements the converge.EventSink interface. It's used
// to handle converge events during configuration updates.
type configEventSink struct {
	eventSender events.EventSender
}

// newConfigEventSink returns a new instance of a configEventSink
func newConfigEventSink(sender events.EventSender) *configEventSink {
	return &configEventSink{
		eventSender: sender,
	}
}

// Sink handles converge events during configuration change operations. As most
// converge events are NOOP we'll only report events that are modifications
// or failures.
func (sink *configEventSink) Sink(e converge.Event) {
	finishEvent, ok := e.(converge.ServiceFinishEvent)
	if !ok {
		return
	}

	pkg := finishEvent.Pkg()
	var phaseID api.DeployEvent_PhaseID

	switch e.(type) {
	case *converge.InstallServiceStarted, *converge.InstallServiceFinished:
		phaseID = events.InstallServicePhase
	case *converge.ConfigureServiceStarted, *converge.ConfigureServiceFinished:
		phaseID = events.InitServicePhase
	case *converge.RunServiceStarted, *converge.RunServiceFinished:
		phaseID = events.StartServicePhase
	case *converge.UnloadServiceStarted, *converge.UnloadServiceFinished:
		phaseID = events.UnloadServicePhase
	default:
		// HACK: It shouldn't be possible to get here but just in case we'll
		// make sure our phase is a real phase ID.
		phaseID = events.InitServicePhase
	}

	if errMsg := finishEvent.Error(); errMsg != nil {
		sink.eventSender.PhaseStep(api.CompleteFail, phaseID, pkg.Name(), errMsg.Error())
	}

	if finishEvent.Modified() {
		sink.eventSender.PhaseStep(api.CompleteOk, phaseID, pkg.Name(), "")
	}
}

func (s *server) updateUserOverrideConfigFromRestoreBackupRequest(req *api.RestoreBackupRequest) error {
	// If the user supplied AWS creds or bucket info that is different from the
	// values that are persisted in the deployment-service bolt db we need to
	// update them before we bootstrap backup-gateway. If we don't do that the
	// bgw may be configured to use the wrong bucket or credentials.

	cfg, err := s.deployment.GetUserOverrideConfigForPersistence().NewDeepCopy()
	if err != nil {
		return status.Error(codes.FailedPrecondition, errors.Wrap(err, "copying existing config").Error())
	}

	if err = api.MergeAndValidateNewUserOverrideConfig(cfg, req.GetRestore()); err != nil {
		return status.Error(codes.InvalidArgument, errors.Wrap(err, "updating config").Error())
	}

	if err = s.deployment.ReplaceUserOverrideConfig(cfg, s.secretStore); err != nil {
		return status.Error(codes.Internal, errors.Wrap(err, "replacing config").Error())
	}

	if err = s.updateExpectedServices(); err != nil {
		return status.Error(codes.Internal, err.Error())
	}

	if err = s.persistDeployment(); err != nil {
		return status.Error(codes.Internal, err.Error())
	}

	return nil
}

//setConfigForRedirectLogs Add the config for rsyslog and logrotate
//if the req has redirect_sys_log as true and existing has redirect_sys_log as false it will add the configurations
//if the req has redirect_sys_log as false and existing has redirect_sys_log as true it will remove the configurations
func setConfigForRedirectLogs(req *api.PatchAutomateConfigRequest, existingCopy *deployment.AutomateConfig) error {

	//Set the config if already not set
	if req.GetConfig().GetGlobal().GetV1().GetLog().GetRedirectSysLog().GetValue() == true &&
		existingCopy.GetGlobal().GetV1().GetLog().GetRedirectSysLog().GetValue() == false {

		err := createConfigFileForAutomateSysLog(req.GetConfig().GetGlobal().GetV1().GetLog().GetRedirectLogFilePath().GetValue())
		if err != nil {
			return status.Error(codes.Internal, err.Error())
		}
		err = restartSyslogService()
		if err != nil {
			return status.Error(codes.Internal, err.Error())
		}

		if err = runLogrotateConfig(req); err != nil {
			logrus.Errorf("cannot configure log rotate: %v", err)
			return err
		}
	}

	//Set the config if already not set
	if req.GetConfig().GetGlobal().GetV1().GetLog().GetRedirectSysLog().GetValue() == true &&
		existingCopy.GetGlobal().GetV1().GetLog().GetRedirectSysLog().GetValue() == true {

		err := createConfigFileForAutomateSysLog(req.GetConfig().GetGlobal().GetV1().GetLog().GetRedirectLogFilePath().GetValue())
		if err != nil {
			return status.Error(codes.Internal, err.Error())
		}

		if req.GetConfig().GetGlobal().GetV1().GetLog().GetRedirectLogFilePath().GetValue() != existingCopy.GetGlobal().GetV1().GetLog().GetRedirectLogFilePath().GetValue() {
			err = restartSyslogService()
			if err != nil {
				return status.Error(codes.Internal, err.Error())
			}
		}

		res, err := UpdateOfLogroateConfig(req, existingCopy)
		if err != nil {
			logrus.Errorf("cannot merge requested and existing structs through mergo.Merge: %v", err)
		}

		if err = runLogrotateConfig(res); err != nil {
			logrus.Errorf("cannot configure log rotate: %v", err)
			return err
		}
	}

	//Rollback the config if requested
	if req.GetConfig().GetGlobal().GetV1().GetLog().GetRedirectSysLog().GetValue() == false &&
		existingCopy.GetGlobal().GetV1().GetLog().GetRedirectSysLog().GetValue() == true {
		err := removeConfigFileForAutomateSyslog()
		if err != nil {
			return status.Error(codes.Internal, "Failed to remove configuration into syslog")
		}
		err = restartSyslogService()
		if err != nil {
			return status.Error(codes.Internal, err.Error())
		}

		if err = rollbackLogrotate(); err != nil {
			logrus.Errorf("cannot rollback logrotate: %v", err)
			return err
		}

	}

	return nil
}

//restartSyslogService restarts the rsyslog service for the supported platforms
func restartSyslogService() error {
	_, err := exec.Command("bash", "-c", "systemctl restart rsyslog.service").Output()
	if err != nil {
		return status.Error(codes.Internal, errors.Wrap(err, "Unable to restart rsyslog").Error())
	}

	return nil
}

//createConfigFileForAutomateSysLog created a config file as /etc/rsyslog.d/automate.conf
// which redirects the logs to the specified location
func createConfigFileForAutomateSysLog(pathForLog string) error {
	os.Remove(rsyslogConfigFile)
	f, err := os.Create(rsyslogConfigFile)

	if err != nil {
		return status.Error(codes.Internal, errors.Wrap(err, "Unable to create rsyslog configuration file for automate").Error())
	}

	defer f.Close()

	_, err2 := f.WriteString(fmt.Sprintf("if $programname == 'hab' then %s\n& stop\n", getLogFileName(pathForLog)))

	if err2 != nil {
		return status.Error(codes.Internal, errors.Wrap(err, "Unable to write in  rsyslog configuration file for automate").Error())
	}

	return nil

}

//removeConfigFileForAutomateSyslog Deletes the file /etc/rsyslog.d/automate.conf, to disable redirecting logs
func removeConfigFileForAutomateSyslog() error {
	err := os.Remove(rsyslogConfigFile)
	if err != nil {
		return status.Error(codes.Internal, errors.Wrap(err, "Unable to delete the  rsyslog configuration file for automate").Error())
	}

	return nil
}

// runLogrotateConfig() to initiate logrotate setup
func runLogrotateConfig(req *api.PatchAutomateConfigRequest) error {
	if err := logrotateConfChecks(); err != nil {
		logrus.Error("Logrotate isn't setup!")
	}

	if err := configLogrotate(req.GetConfig().GetGlobal().GetV1().GetLog()); err != nil {
		logrus.Errorf("Error while configuring logrotate: %v", err)
		return err
	}

	return nil
}

//logrotateConfChecks Checks if the supported platform has logrotate pre-installed or not
//if not, it returns and error which depicts to install the logrotate
func logrotateConfChecks() error {
	_, err := exec.Command("logrotate").Output()
	if strings.Contains(err.Error(), "executable file not found") {
		log.Printf("The system doesn't have logrotate installed, %v", err)
		return err
	}

	// Check for the logrotate.conf and rsyslog.conf existance
	if _, err := os.Stat("/etc/logrotate.conf"); errors.Is(err, os.ErrNotExist) {
		log.Printf("The system doesn't seem to have logrotate installed or configured: %v", err)
		return err
	}
	return nil
}

// configLogrotate Adds a config file for logrotate as /etc/logrotate.d/automate
// it handles all the config for rotating logs.
func configLogrotate(req *config.Log) error {
	var logRotateConfigContent string
	// Create a file in /etc/logrotate.d/automate
	os.Remove(logRotateConfigFile)
	file, err := os.Create(logRotateConfigFile)
	if err != nil {
		logrus.Errorf("cannot create a file: %v", err)
		return err
	}
	defer file.Close()

	if req.GetCompressRotatedLogs().GetValue() == true {
		logRotateConfigContent = LogRotateConf(getLogFileName(req.GetRedirectLogFilePath().GetValue()),
			getConcatStringFromConfig("size", req.GetMaxSizeRotateLogs().GetValue()), getConcatStringFromConfig("rotate", req.GetMaxNumberRotatedLogs().GetValue()), "missingok", "copytruncate", "compress", "dateext")
	} else {
		logRotateConfigContent = LogRotateConf(getLogFileName(req.GetRedirectLogFilePath().GetValue()),
			getConcatStringFromConfig("size", req.GetMaxSizeRotateLogs().GetValue()), getConcatStringFromConfig("rotate", req.GetMaxNumberRotatedLogs().GetValue()), "missingok", "copytruncate", "dateext")
	}

	// Write the byteSlice to file
	noOfBytes, err := file.WriteString(logRotateConfigContent)
	if err != nil {
		logrus.Errorf("cannot write the byte slice to the file: %v", err)
		return err
	}
	logrus.Infof("%v no of bytes are written to the file", noOfBytes)

	return nil
}

func rollbackLogrotate() error {
	return os.Remove(logRotateConfigFile)
}

func UpdateOfLogroateConfig(req *api.PatchAutomateConfigRequest, existingCopy *deployment.AutomateConfig) (*api.PatchAutomateConfigRequest, error) {
	if req.GetConfig().GetGlobal().GetV1().GetLog().GetRedirectSysLog().GetValue() == true && existingCopy.GetGlobal().GetV1().GetLog().GetRedirectSysLog().GetValue() == true {
		if err := mergo.Merge(req.Config, existingCopy); err != nil {
			logrus.Errorf("cannot merge the requested and existing structs: %v", err)
			return nil, err
		}
	}

	return req, nil
}
