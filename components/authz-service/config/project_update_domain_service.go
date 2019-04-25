package config

import "time"

type ProjectUpdateDomainService struct {
	Name                   string    `toml:"name"`
	PercentageComplete     float64   `toml:"percentage_complete"`
	EstimatedTimeCompelete time.Time `toml:"estimated_time_compelete"`
	LastUpdate             time.Time `toml:"last_update"`
	Complete               bool      `toml:"complete"`
	Failed                 bool      `toml:"failed"`
	FailureMessage         string    `toml:"failure_message"`
}

func (domainService ProjectUpdateDomainService) copy() ProjectUpdateDomainService {
	return ProjectUpdateDomainService{
		Name:                   domainService.Name,
		PercentageComplete:     domainService.PercentageComplete,
		EstimatedTimeCompelete: domainService.EstimatedTimeCompelete,
		LastUpdate:             domainService.LastUpdate,
		Complete:               domainService.Complete,
		Failed:                 domainService.Failed,
		FailureMessage:         domainService.FailureMessage,
	}
}

func (domainService1 ProjectUpdateDomainService) equal(
	domainService2 ProjectUpdateDomainService) bool {
	if domainService1.Name != domainService2.Name {
		return false
	}

	if domainService1.PercentageComplete != domainService2.PercentageComplete {
		return false
	}

	if domainService1.EstimatedTimeCompelete != domainService2.EstimatedTimeCompelete {
		return false
	}

	if domainService1.LastUpdate != domainService2.LastUpdate {
		return false
	}

	if domainService1.Complete != domainService2.Complete {
		return false
	}

	if domainService1.Failed != domainService2.Failed {
		return false
	}

	if domainService1.FailureMessage != domainService2.FailureMessage {
		return false
	}

	return true
}
