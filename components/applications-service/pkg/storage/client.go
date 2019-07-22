package storage

import (
	"fmt"
	"time"

	"github.com/chef/automate/api/external/habitat"
)

type Client interface {
	// @param (event)
	IngestHealthCheckEvent(*habitat.HealthCheckEvent) error
	// @param (name, member_id)
	GetServiceFromUniqueFields(string, string) (*Service, bool)
	// @param (sortField, sortAsc, page, pageSize, filters)
	GetServices(string, bool, int32, int32, map[string][]string) ([]*Service, error)
	// @param (thresholdMinutes)
	GetDisconnectedServices(int32) ([]*Service, error)
	// @param (thresholdMinutes)
	DeleteDisconnectedServices(int32) ([]*Service, error)
	// @param (sortField, sortAsc, page, pageSize, filters)
	GetServiceGroups(string, bool, int32, int32, map[string][]string) ([]*ServiceGroupDisplay, error)
	// @param (id)
	ServiceGroupExists(string) (string, bool)
	// @param (filters)
	GetServicesHealthCounts(map[string][]string) (*HealthCounts, error)
	GetServiceGroupsHealthCounts() (*HealthCounts, error)
	// @param (id)
	GetDeployment(int32) (*Deployment, error)
	// @param (id)
	GetSupervisor(int32) (*Supervisor, error)

	GetServicesCount() (int32, error)
	GetServiceGroupsCount() (int32, error)
	GetSupervisorsCount() (int32, error)

	// Used by our Integration Tests
	EmptyStorage() error
}

const (
	Critical = "CRITICAL"
	Warning  = "WARNING"
	Unknown  = "UNKNOWN"
	Ok       = "OK"
	None     = "NONE"
)

type UpdateStrategy int

const (
	NoneStrategy UpdateStrategy = iota
	AtOnceStrategy
	RollingStrategy
)

func (x UpdateStrategy) String() string {
	switch x {
	case AtOnceStrategy:
		return "AT-ONCE"
	case RollingStrategy:
		return "ROLLING"
	default:
		return "NONE"
	}
}

func HabitatUpdateStrategyToStorageFormat(strategy habitat.UpdateStrategy) UpdateStrategy {
	switch strategy {
	case habitat.UpdateStrategy_AtOnce:
		return AtOnceStrategy
	case habitat.UpdateStrategy_Rolling:
		return RollingStrategy
	default:
		return NoneStrategy
	}
}

type Service struct {
	ID                  int32
	SupMemberID         string
	Origin              string
	Name                string
	Version             string
	Release             string
	Status              string
	Health              string
	Group               string
	Fqdn                string
	Application         string
	Environment         string
	Channel             string
	Site                string
	PreviousHealth      string
	UpdateStrategy      string
	LastEventOccurredAt time.Time
	HealthUpdatedAt     time.Time
}

func (s *Service) FullReleaseString() string {
	return fmt.Sprintf("%s/%s/%s/%s",
		s.Origin, s.Name, s.Version, s.Release)
}

type ServiceGroupDisplay struct {
	ID                   int32
	Name                 string
	DeploymentID         int32
	Package              string
	Release              string
	HealthStatus         string
	HealthPercentage     int32
	ServicesHealthCounts HealthCounts
	Application          string
	Environment          string
}

type Supervisor struct {
	ID       int32
	MemberID string
	Fqdn     string
	Site     string
}

type Deployment struct {
	ID          int32
	Application string
	Environment string
}

type HealthCounts struct {
	Total    int32
	Ok       int32
	Warning  int32
	Critical int32
	Unknown  int32
}
