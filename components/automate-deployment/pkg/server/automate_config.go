package server

import (
	"context"
	"fmt"
	"github.com/chef/automate/api/config/deployment"
	api "github.com/chef/automate/api/interservice/deployment"
	"github.com/chef/automate/components/automate-deployment/pkg/converge"
	"github.com/chef/automate/components/automate-deployment/pkg/events"
	"github.com/pkg/errors"
	"os"
	"os/exec"

	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/status"
)

var rsyslogConfigFile = "/etc/rsyslog.d/automate.conf"

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

		fmt.Println("Existing value ", existingCopy.GetGlobal().GetV1().GetLog().GetRedirectSysLog().GetValue())

		// Compare our existing configurations hash with the hash of the configuration
		// that was returned to the client when the request was initiated. If
		// they don't match then the configuration changed before they submitted
		// the configuration update.
		if existingHash != req.Hash {
			return status.Error(codes.DeadlineExceeded, "The configuration has changed since you initiated the update request. Please try again.")
		}

		if err = setConfigForRedirectLogs(req, existingCopy); err != nil {
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

func setConfigForRedirectLogs(req *api.PatchAutomateConfigRequest, existingCopy *deployment.AutomateConfig) error {
	//Set the config if already not set
	if req.GetConfig().GetGlobal().GetV1().GetLog().GetRedirectSysLog().GetValue() == true &&
		existingCopy.GetGlobal().GetV1().GetLog().GetRedirectSysLog().GetValue() == false {
		err := createConfigFileForAutomateSysLog()
		if err != nil {
			return status.Error(codes.Internal, "Failed to create configuration into syslog")
		}
		err = restartSyslogService()
		if err != nil {
			return status.Error(codes.Internal, "Failed to restart syslog service")
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
			return status.Error(codes.Internal, "Failed to restart syslog service while removing config file for syslog")
		}

	}

	return nil
}

func restartSyslogService() error {
	_, err := exec.Command("bash", "-c", "systemctl restart rsyslog.service").Output()
	if err != nil {
		fmt.Println("Unable to restart syslog service with error ", err)
		return status.Error(codes.Internal, errors.Wrap(err, "Unable to restart rsyslog").Error())
	}

	return nil
}

func createConfigFileForAutomateSysLog() error {
	f, err := os.Create(rsyslogConfigFile)

	if err != nil {
		return status.Error(codes.Internal, errors.Wrap(err, "Unable to create rsyslog configuration file for automate").Error())
	}

	defer f.Close()

	_, err2 := f.WriteString("if $programname == 'hab' then /var/log/automate.log\n& stop\n")

	if err2 != nil {
		fmt.Println("Unable to create config with error ", err2)
		return status.Error(codes.Internal, errors.Wrap(err, "Unable to write in  rsyslog configuration file for automate").Error())
	}

	return nil

}

func removeConfigFileForAutomateSyslog() error {
	err := os.Remove(rsyslogConfigFile)
	if err != nil {
		return status.Error(codes.Internal, errors.Wrap(err, "Unable to delete the  rsyslog configuration file for automate").Error())
	}

	return nil
}
