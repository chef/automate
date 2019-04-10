package deployment

import (
	"fmt"
	"strings"
	"time"

	"github.com/golang/protobuf/ptypes"
)

const (
	// Waiting status means we haven't started a deploy, a phase,
	// or a phase step
	Waiting DeployEvent_Status = DeployEvent_WAITING

	// Running status means we are in progress with a deploy, a
	// phase, or a phase step
	Running DeployEvent_Status = DeployEvent_RUNNING

	// CompleteOk status means a deploy, phase, or phase step
	// finished successfully.
	CompleteOk DeployEvent_Status = DeployEvent_COMPLETE_OK

	// CompleteFail status means that a deploy, phase, or phase
	// step ran and failed.
	CompleteFail DeployEvent_Status = DeployEvent_COMPLETE_FAIL

	// SelfUpgradePending means that the deployment service needs to
	// upgrade itself.
	SelfUpgradePending DeployEvent_Status = DeployEvent_SELF_UPGRADE_PENDING

	// SelfReconfigurePending means that the deployment service needs
	// to reconfigure itself.
	SelfReconfigurePending DeployEvent_Status = DeployEvent_SELF_RECONFIGURE_PENDING

	// NoPrint is what Format() returns when the caller shouldn't
	// print anything.
	//
	// HACK: eventually the CLI writer suppresses empty strings so
	// this squelches "Deploy started" and other messages we would
	// like to avoid for UI reasons.
	NoPrint = ""
)

// NewEvent creates a new DeployEvent wrapping any of the DeployEvent subtypes
// defined in the proto
func NewEvent(seq uint64, id string, timeStamp time.Time, event isDeployEvent_Event) DeployEvent {
	ts, _ := ptypes.TimestampProto(timeStamp)
	return DeployEvent{
		Sequence:     seq,
		DeploymentId: id,
		Time:         ts,
		Event:        event,
	}
}

// Format returns a string representation of an DeployEvent suitable
// for showing to a user.
func (event *DeployEvent) Format() string {
	var ret string

	switch e := event.Event.(type) {
	case *DeployEvent_Deploy_:
		switch e.Deploy.Status {
		case Running:
			ret = NoPrint
		case CompleteFail:
			ret = "Deploy failed"
		case CompleteOk:
			ret = NoPrint
		case SelfUpgradePending:
			ret = `The requested operation has not fully completed because the
deployment-service must upgrade itself first. The operation will
complete after the deployment-service restarts.`
		case SelfReconfigurePending:
			ret = `The requested operation has not fully completed because the
deployment-service must configure itself first. The operation will
complete after the deployment-service restarts (if necessary) or the
next time the deployment-service converges Chef Automate's state.
`
		default:
			ret = fmt.Sprintf("FOUND UNEXPECTED DEPLOY STATUS %s", DeployEvent_Status_name[int32(e.Deploy.Status)])
		}
	case *DeployEvent_Phase_:
		switch e.Phase.Status {
		case Running:
			ret = formatPhase(e.Phase.PhaseId)
		case CompleteOk, CompleteFail:
			ret = NoPrint
		default:
			ret = fmt.Sprintf("FOUND UNEXPECTED PHASE STATUS %s", DeployEvent_Status_name[int32(e.Phase.Status)])
		}
	case *DeployEvent_PhaseStep_:
		switch e.PhaseStep.Status {
		case Running:
			ret = formatPhaseStep(e.PhaseStep.PhaseId, e.PhaseStep.StepName)
		case CompleteOk:
			ret = NoPrint
		case CompleteFail:
			ret = e.PhaseStep.Error
		default:
			ret = fmt.Sprintf("FOUND UNEXPECTED PHASE STEP STATUS %s", DeployEvent_Status_name[int32(e.PhaseStep.Status)])
		}
	case *DeployEvent_Backup_:
		switch e.Backup.Status {
		case Running:
			m := make([]string, len(e.Backup.Operations))
			for i, o := range e.Backup.Operations {
				m[i] = fmt.Sprintf("%s (sync %.2f%%) (async %.2f%%)", o.Name, o.SyncProgress, o.AsyncProgress)
			}

			ret = strings.Join(m, "\n")
		case CompleteOk:
			ret = ""
		case CompleteFail:
			for _, o := range e.Backup.Operations {
				if o.Error != "" {
					ret = o.Error
				}
			}
			if ret == "" {
				ret = "Backup CompleteFail"
			}
		}
	case *DeployEvent_TaskComplete_:
		ret = NoPrint
	case nil:
		ret = "MISSING EVENT"
	default:
		ret = fmt.Sprintf("UNKNOWN EVENT TYPE: %v %+v\n", e, event)
	}

	return ret
}

func formatPhase(phaseID DeployEvent_PhaseID) string {
	switch phaseID {
	case DeployEvent_INIT_SERVICE, DeployEvent_INSTALL_SERVICE, DeployEvent_START_SERVICE:
		return NoPrint
	case DeployEvent_CHECK_SERVICE_HEALTH:
		return "Checking service health"
	case DeployEvent_CREATE_ADMIN_USER:
		return "Creating admin user"
	case DeployEvent_APPLY_LICENSE:
		return "Applying License"
	default:
		return fmt.Sprintf("UNKNOWN PHASE: %s", DeployEvent_PhaseID_name[int32(phaseID)])
	}
}

func formatPhaseStep(phaseID DeployEvent_PhaseID, svcName string) string {
	switch phaseID {
	case DeployEvent_INIT_SERVICE:
		return fmt.Sprintf("Configuring %s", svcName)
	case DeployEvent_INSTALL_SERVICE:
		return fmt.Sprintf("Installing %s", svcName)
	case DeployEvent_START_SERVICE:
		return fmt.Sprintf("Starting %s", svcName)
	case DeployEvent_CHECK_SERVICE_HEALTH:
		return "."
	default:
		return fmt.Sprintf("UNKNOWN PHASE IN PHASE STEP: %s", DeployEvent_PhaseID_name[int32(phaseID)])
	}
}
