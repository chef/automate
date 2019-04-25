package config

import (
	"errors"
	"fmt"
	"time"

	event_ids "github.com/chef/automate/lib/event"
)

// ProjectUpdateStage - the config for project updating
// The stage is all the data that needs to be remembered if the service is restarted.
type ProjectUpdateStage struct {
	State           string                       `toml:"state"`
	ProjectUpdateID string                       `toml:"project_update_id"`
	DomainServices  []ProjectUpdateDomainService `toml:"domain_services"`
	FailureMessage  string                       `toml:"failure_message"`
	Failed          bool                         `toml:"failed"`
}

func (stage ProjectUpdateStage) Copy() ProjectUpdateStage {
	domainServices := make([]ProjectUpdateDomainService, len(stage.DomainServices))
	for index, dService := range stage.DomainServices {
		domainServices[index] = dService.copy()
	}

	return ProjectUpdateStage{
		State:           stage.State,
		ProjectUpdateID: stage.ProjectUpdateID,
		DomainServices:  domainServices,
		FailureMessage:  stage.FailureMessage,
		Failed:          stage.Failed,
	}
}

func (stage ProjectUpdateStage) StopRunning() ProjectUpdateStage {
	stage.DomainServices = []ProjectUpdateDomainService{
		ProjectUpdateDomainService{
			Name:       event_ids.ComplianceInspecReportProducerID,
			LastUpdate: time.Now(),
		},
		ProjectUpdateDomainService{
			Name:       event_ids.InfraClientRunsProducerID,
			LastUpdate: time.Now(),
		},
	}
	stage.State = NotRunningState

	return stage
}

func (stage ProjectUpdateStage) StartRunning(projectUpdateID string) ProjectUpdateStage {
	stage.Failed = false
	stage.FailureMessage = ""
	stage.ProjectUpdateID = projectUpdateID
	stage.DomainServices = []ProjectUpdateDomainService{
		ProjectUpdateDomainService{
			Name:       event_ids.ComplianceInspecReportProducerID,
			LastUpdate: time.Now(),
		},
		ProjectUpdateDomainService{
			Name:       event_ids.InfraClientRunsProducerID,
			LastUpdate: time.Now(),
		},
	}
	stage.State = RunningState

	return stage
}

func (stage ProjectUpdateStage) AreDomainServicesComplete() bool {
	for _, domainService := range stage.DomainServices {
		if !domainService.Complete {
			return false
		}
	}
	return true
}

func (stage ProjectUpdateStage) HasFailedDomainService() bool {
	failure := false
	for _, domainService := range stage.DomainServices {
		if domainService.Failed {
			failure = true
		}
	}

	return failure
}

func (stage ProjectUpdateStage) OldestDomainServiceUpdateTime() time.Time {
	oldestTime := time.Now()
	for _, domainService := range stage.DomainServices {
		if domainService.LastUpdate.Before(oldestTime) {
			oldestTime = domainService.LastUpdate
		}
	}

	return oldestTime
}

func (stage1 ProjectUpdateStage) Equal(stage2 ProjectUpdateStage) bool {
	if stage1.State != stage2.State {
		return false
	}

	if stage1.Failed != stage2.Failed {
		return false
	}

	if stage1.ProjectUpdateID != stage2.ProjectUpdateID {
		return false
	}

	if stage1.FailureMessage != stage2.FailureMessage {
		return false
	}

	if len(stage1.DomainServices) != len(stage2.DomainServices) {
		return false
	}

	for index, domainService1 := range stage1.DomainServices {
		if !domainService1.equal(stage2.DomainServices[index]) {
			return false
		}
	}

	return true
}

func (stage ProjectUpdateStage) FindDomainServiceIndex(producerID string) (int, error) {
	for index, domainService := range stage.DomainServices {
		if domainService.Name == producerID {
			return index, nil
		}
	}

	return -1, errors.New(fmt.Sprintf(
		"Domain service not found with producerID %q", producerID))
}
