package server

import (
	"context"

	"github.com/chef/automate/api/config/deployment"

	api "github.com/chef/automate/api/interservice/deployment"
	"github.com/chef/automate/components/automate-deployment/pkg/converge"
	"github.com/chef/automate/components/automate-deployment/pkg/events"

	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/status"
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

		if err = existingCopy.OverrideConfigValues(req.Config); err != nil {
			return status.Error(codes.Internal, "Failed to merge configuration into existing configuration")
		}

		if err = existingCopy.ValidateWithGlobalAndDefaults(); err != nil {
			return status.Error(codes.InvalidArgument, err.Error())
		}

		if err = s.deployment.ReplaceUserOverrideConfig(existingCopy); err != nil {
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

		if err = s.deployment.ReplaceUserOverrideConfig(req.Config); err != nil {
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

	var reqCfg *deployment.AutomateConfig
	var curCfg *deployment.AutomateConfig
	if req.GetRestore().GetSetConfig() != nil {
		curCfg = req.GetRestore().GetSetConfig()
	} else {
		var err error
		curCfg, err = s.deployment.GetUserOverrideConfigForPersistence().NewDeepCopy()
		if err != nil {
			return status.Error(codes.FailedPrecondition, "Failed to load configuration")
		}
	}

	reqCfg = api.NewUserOverrideConfigFromBackupRestoreTask(req.GetRestore())

	if err := curCfg.OverrideConfigValues(reqCfg); err != nil {
		return status.Error(codes.Internal, "Failed to set backup configuration")
	}

	if err := curCfg.ValidateWithGlobalAndDefaults(); err != nil {
		return status.Error(codes.InvalidArgument, err.Error())
	}

	if err := s.deployment.ReplaceUserOverrideConfig(curCfg); err != nil {
		return status.Error(codes.Internal, err.Error())
	}

	if err := s.updateExpectedServices(); err != nil {
		return status.Error(codes.Internal, err.Error())
	}

	if err := s.persistDeployment(); err != nil {
		return status.Error(codes.Internal, err.Error())
	}

	return nil
}
