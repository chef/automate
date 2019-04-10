package server

import (
	"github.com/sirupsen/logrus"

	api "github.com/chef/automate/api/interservice/deployment"
	"github.com/chef/automate/components/automate-deployment/pkg/converge"
	"github.com/chef/automate/components/automate-deployment/pkg/events"
)

type eventAdapter struct {
	eventSender events.EventSender
	lastPhase   *api.DeployEvent_PhaseID
}

func newEventAdapter(eventSender events.EventSender) converge.EventSink {
	return &eventAdapter{
		eventSender: eventSender,
	}
}

func (adapter *eventAdapter) updatePhase(phaseID api.DeployEvent_PhaseID) {
	if adapter.lastPhase == nil {
		adapter.eventSender.Phase(api.Running, phaseID)
		adapter.lastPhase = new(api.DeployEvent_PhaseID)
		*adapter.lastPhase = phaseID
		return
	}

	if *adapter.lastPhase != phaseID {
		adapter.eventSender.Phase(api.CompleteOk, *adapter.lastPhase)
		*adapter.lastPhase = phaseID
		adapter.eventSender.Phase(api.Running, *adapter.lastPhase)
	}
}

func (adapter *eventAdapter) Sink(e converge.Event) {
	switch ev := e.(type) {
	case *converge.ConvergerStarted:
	case *converge.ConvergerFinished:
		if adapter.lastPhase != nil {
			adapter.eventSender.Phase(api.CompleteFail, *adapter.lastPhase)
		}
	case converge.ServiceEvent:
		pkg := ev.Pkg()
		var phaseID api.DeployEvent_PhaseID

		switch e.(type) {
		case *converge.InstallServiceStarted, *converge.InstallServiceFinished:
			phaseID = events.InstallServicePhase
		case *converge.ConfigureServiceStarted, *converge.ConfigureServiceFinished:
			//TODO(jaym) InitServicePhase renders as Configuring, but the constant does
			//           not match what it does
			phaseID = events.InitServicePhase
		case *converge.RunServiceStarted, *converge.RunServiceFinished:
			phaseID = events.StartServicePhase
		// TODO(jaym) add remove phase
		default:
			logrus.WithField("event", e).Debug("Unknown event type")
			return
		}

		adapter.updatePhase(phaseID)

		switch startOrFinish := ev.(type) {
		case converge.ServiceStartEvent:
			adapter.eventSender.PhaseStep(api.Running, phaseID, pkg.Name(), "")
		case converge.ServiceFinishEvent:
			err := startOrFinish.Error()
			if err != nil {
				adapter.eventSender.PhaseStep(api.CompleteFail, phaseID, pkg.Name(), err.Error())
			} else {
				adapter.eventSender.PhaseStep(api.CompleteOk, phaseID, pkg.Name(), "")
			}
		default:
			logrus.WithField("event", e).Debug("Unknown event type")
			return
		}
	case converge.SupervisorUpgradeEvent:
		// TODO(ssd) 2018-06-21: Ignored for now
	}
}
