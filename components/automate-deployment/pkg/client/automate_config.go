package client

import (
	"context"
	"fmt"
	"time"

	"github.com/pkg/errors"

	dc "github.com/chef/automate/api/config/deployment"
	api "github.com/chef/automate/api/interservice/deployment"
	"github.com/chef/automate/components/automate-cli/pkg/status"
	"github.com/chef/automate/components/automate-deployment/pkg/cli"
)

// GetAutomateConfig makes a gRPC request to the server and returns the
// userOverrideConfig.
func GetAutomateConfig(timeout int64) (*api.GetAutomateConfigResponse, error) {
	con, err := Connection(time.Duration(timeout) * time.Second)

	if err != nil {
		return &api.GetAutomateConfigResponse{}, err
	}

	return con.GetAutomateConfig(context.Background(), &api.GetAutomateConfigRequest{})
}

// PatchAutomateConfig makes a gRPC request to the server with a given
// userOverrideConfig. The server will validate, merge, and persist the
// configuration. After it it persisted it will start a converge task
// and return the task ID to the client. It then streams the converge
// events to the given writer.
func PatchAutomateConfig(timeout int64, config *dc.AutomateConfig, writer cli.FormatWriter) error {
	writer.Title("Updating deployment configuration")

	con, err := Connection(time.Duration(timeout) * time.Second)
	if err != nil {
		return err
	}

	getRes, err := GetAutomateConfig(timeout)
	if err != nil {
		return status.Wrap(err, status.DeploymentServiceCallError, "Failed attempting to get Chef Automate configuration from the deployment-service")
	}

	// TODO: Create merge configuration together, show it to the user and
	// confirm that they wish to proceed with the patched merged config.

	patchReq := &api.PatchAutomateConfigRequest{
		Config: config,
		Hash:   getRes.Hash,
	}

	patchRes, err := con.PatchAutomateConfig(context.Background(), patchReq)
	if err != nil {
		return status.Wrap(err, status.DeploymentServiceCallError, "Failed attempting to patch Chef Automate configuration")
	}

	writer.Title("Applying deployment configuration")

	h := configEventHandler{Writer: writer}
	if err := con.StreamDeployEvents(patchRes.TaskId, &api.DeploymentID{}, &h); err != nil {
		return status.Wrap(err, status.DeploymentServiceCallError, "Failed to stream configuration events")
	}

	return nil
}

// SetAutomateConfig makes a gRPC request to the server with a given
// userOverrideConfig. The server will validate, set, and persist the
// configuration. After it it persisted it will start a converge task
// and return the task ID to the client. It then streams the converge
// events to the given writer.
func SetAutomateConfig(timeout int64, config *dc.AutomateConfig, writer cli.FormatWriter) error {
	writer.Title("Setting deployment configuration")

	con, err := Connection(time.Duration(timeout) * time.Second)
	if err != nil {
		return err
	}

	req := &api.SetAutomateConfigRequest{
		Config: config,
	}

	res, err := con.SetAutomateConfig(context.Background(), req)
	if err != nil {
		return status.Wrap(err, status.DeploymentServiceCallError, "Failed attempting to set Chef Automate configuration")
	}

	writer.Title("Applying deployment configuration")

	h := configEventHandler{Writer: writer}
	if err := con.StreamDeployEvents(res.TaskId, &api.DeploymentID{}, &h); err != nil {
		return status.Wrap(err, status.DeploymentServiceCallError, "Failed to stream configuration events")
	}

	return nil
}

// configEventHandler is an implementation of DeployEventHandler for the config
// set and patch operations.
type configEventHandler struct {
	Writer cli.FormatWriter
}

// HandleEvent filters the post config update events and fails if an error
// occurs.
func (ceh *configEventHandler) HandleEvent(event *api.DeployEvent) {
	switch e := event.Event.(type) {
	case *api.DeployEvent_PhaseStep_:
		switch e.PhaseStep.Status {
		case CompleteFail:
			ceh.formatAndWriteFail(e.PhaseStep.PhaseId, e.PhaseStep.StepName)
		case CompleteOk:
			ceh.formatAndWriteOk(e.PhaseStep.PhaseId, e.PhaseStep.StepName)
		default:
		}
	default:
	}
}

func (ceh *configEventHandler) formatAndWriteFail(phaseID api.DeployEvent_PhaseID, stepName string) {
	action := ""
	switch phaseID {
	case api.DeployEvent_INIT_SERVICE:
		action = "configure"
	case api.DeployEvent_INSTALL_SERVICE:
		action = "install"
	case api.DeployEvent_START_SERVICE:
		action = "start"
	case api.DeployEvent_UNLOAD_SERVICE:
		action = "unload"
	default:
		return
	}

	ceh.Writer.FailCause(errors.Errorf("Failed to %s %s after configuration update", action, stepName))
}

func (ceh *configEventHandler) formatAndWriteOk(phaseID api.DeployEvent_PhaseID, stepName string) {
	action := ""
	switch phaseID {
	case api.DeployEvent_INIT_SERVICE:
		action = "Configured"
	case api.DeployEvent_INSTALL_SERVICE:
		action = "Installed"
	case api.DeployEvent_START_SERVICE:
		action = "Started"
	case api.DeployEvent_UNLOAD_SERVICE:
		action = "Unloaded"
	default:
		return
	}

	ceh.Writer.Body(fmt.Sprintf("%s %s", action, stepName))
}
