package events

import (
	"time"

	api "github.com/chef/automate/api/interservice/deployment"
)

const (
	// InitServicePhase phase for initializing services
	InitServicePhase = api.DeployEvent_INIT_SERVICE
	// InstallServicePhase phase for installing services
	InstallServicePhase = api.DeployEvent_INSTALL_SERVICE
	// StartServicePhase phase for starting services
	StartServicePhase = api.DeployEvent_START_SERVICE
	// UnloadServicePhase phase for unloading a service for restart or removal
	UnloadServicePhase = api.DeployEvent_UNLOAD_SERVICE
	// CheckingServiceHealth phase for checking services (status)
	CheckingServiceHealth = api.DeployEvent_CHECK_SERVICE_HEALTH
	// CreateAdminUser phase for creating initial admin user
	CreateAdminUser = api.DeployEvent_CREATE_ADMIN_USER
	// ApplyLicense phase for applying an A2 License if in config
	ApplyLicense = api.DeployEvent_APPLY_LICENSE
)

// NewDeployEvent returns a new deploy event with the specified status.
func NewDeployEvent(status api.DeployEvent_Status) api.DeployEvent_Deploy {
	return api.DeployEvent_Deploy{
		Status: status,
	}
}

// NewPhaseEvent returns a new phase event with the given name and status.
func NewPhaseEvent(phaseID api.DeployEvent_PhaseID, status api.DeployEvent_Status) api.DeployEvent_Phase {
	return api.DeployEvent_Phase{
		PhaseId: phaseID,
		Status:  status,
	}
}

// NewPhaseStepEvent returns a new phase step event for a given phase,
// step, and status. You can provide an error string if the status is
// CompleteFail.
func NewPhaseStepEvent(
	phaseID api.DeployEvent_PhaseID,
	name string,
	status api.DeployEvent_Status,
	err string) api.DeployEvent_PhaseStep {

	return api.DeployEvent_PhaseStep{
		PhaseId:  phaseID,
		StepName: name,
		Status:   status,
		Error:    err,
	}
}

// NewTaskCompleteEvent creates a new event that when sent, marks the
// task complete
func NewTaskCompleteEvent() api.DeployEvent_TaskComplete {
	return api.DeployEvent_TaskComplete{}
}

// EventForDeploy creates a new event envelope for a deploy event
func EventForDeploy(seq uint64,
	deploymentID string,
	time time.Time,
	event *api.DeployEvent_Deploy) api.DeployEvent {
	return api.NewEvent(seq, deploymentID, time, &api.DeployEvent_Deploy_{Deploy: event})
}

// EventForPhase creates a new event envelope for a phase event
func EventForPhase(seq uint64,
	deploymentID string,
	time time.Time,
	event *api.DeployEvent_Phase) api.DeployEvent {
	return api.NewEvent(seq, deploymentID, time, &api.DeployEvent_Phase_{Phase: event})
}

// EventForPhaseStep creates a new event envelope for a phase step event
func EventForPhaseStep(seq uint64,
	deploymentID string,
	time time.Time,
	event *api.DeployEvent_PhaseStep) api.DeployEvent {
	return api.NewEvent(seq, deploymentID, time, &api.DeployEvent_PhaseStep_{PhaseStep: event})
}

// EventForBackup creates a new event envelope for a backup event
func EventForBackup(seq uint64,
	deploymentID string,
	time time.Time,
	event *api.DeployEvent_Backup) api.DeployEvent {
	return api.NewEvent(seq, deploymentID, time, &api.DeployEvent_Backup_{Backup: event})
}

// EventForTaskComplete creates a new event envelope for a task
// complete event
func EventForTaskComplete(seq uint64,
	deploymentID string,
	time time.Time,
	event *api.DeployEvent_TaskComplete) api.DeployEvent {
	return api.NewEvent(seq, deploymentID, time, &api.DeployEvent_TaskComplete_{TaskComplete: event})
}
