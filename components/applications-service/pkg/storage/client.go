package storage

import (
	"github.com/chef/automate/api/external/applications"
)

type Client interface {
	// @param (event)
	IngestHabEvent(*applications.HabService) error
	GetServiceGroupsHealthCounts() (*HealthCounts, error)
	// @param (sortField, sortAsc, page, pageSize, filters)
	GetServiceGroups(string, bool, int32, int32, map[string][]string) ([]*ServiceGroupDisplay, error)

	// Used by our Integration Tests
	EmptyStorage() error
}

const (
	Critical = "CRITICAL"
	Warning  = "WARNING"
	Unknown  = "UNKNOWN"
	Ok       = "OK"
)

type ServiceGroupDisplay struct {
	ID                   int32
	Name                 string
	DeploymentID         int32
	Release              string
	HealthStatus         string
	HealthPercentage     int32
	ServicesHealthCounts HealthCounts
}

type HealthCounts struct {
	Total    int32
	Ok       int32
	Warning  int32
	Critical int32
	Unknown  int32
}
