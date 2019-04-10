package client

import (
	"github.com/pkg/errors"

	api "github.com/chef/automate/api/interservice/deployment"
	"github.com/chef/automate/components/automate-deployment/pkg/cli"
)

const (
	// Waiting status means we haven't started a deploy, a phase,
	// or a phase step
	Waiting api.DeployEvent_Status = api.DeployEvent_WAITING

	// Running status means we are in progress with a deploy, a
	// phase, or a phase step
	Running api.DeployEvent_Status = api.DeployEvent_RUNNING

	// CompleteOk status means a deploy, phase, or phase step
	// finished successfully.
	CompleteOk api.DeployEvent_Status = api.DeployEvent_COMPLETE_OK

	// CompleteFail status means that a deploy, phase, or phase
	// step ran and failed.
	CompleteFail api.DeployEvent_Status = api.DeployEvent_COMPLETE_FAIL
)

// CLIEventWriter is an implementation of DeployEventHandler that writes the
// events to the terminal
type CLIEventWriter struct {
	Writer cli.FormatWriter
}

// HandleEvent implements the DeployEventHandler contract by writing the events
// to the terminal.
func (ew *CLIEventWriter) HandleEvent(event *api.DeployEvent) {
	switch e := event.Event.(type) {
	case *api.DeployEvent_PhaseStep_:
		switch e.PhaseStep.Status {
		case Running:
			if e.PhaseStep.PhaseId == api.DeployEvent_CHECK_SERVICE_HEALTH {
				ew.Writer.StartSpinner()
			} else {
				ew.Writer.Body(event.Format())
			}
		case CompleteOk:
			if e.PhaseStep.PhaseId == api.DeployEvent_CHECK_SERVICE_HEALTH {
				ew.Writer.StopSpinner()
			}
		case CompleteFail:
			if e.PhaseStep.PhaseId == api.DeployEvent_CHECK_SERVICE_HEALTH {
				ew.Writer.StopSpinner()
			}
			ew.Writer.FailCause(errors.New(e.PhaseStep.Error))
		default:
			ew.Writer.Title(event.Format())
		}
	default:
		format := event.Format()
		if format != "" {
			ew.Writer.Title(format)
		}

	}
}
