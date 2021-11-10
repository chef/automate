package storage

import (
	"context"
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
	// @param (thresholdMinutes)
	MarkDisconnectedServices(int32) ([]*Service, error)
	// @param (service IDs)
	DeleteServicesByID([]string) ([]*Service, error)
	// @param (sortField, sortAsc, page, pageSize, filters)
	GetServiceGroups(string, bool, int32, int32, map[string][]string) ([]*ServiceGroupDisplay, error)
	// @param (fieldName, queryFragment)
	GetServicesDistinctValues(string, string, map[string][]string) ([]string, error)
	// @param (id)
	ServiceGroupExists(string) (string, bool)
	// @param (filters)
	GetServicesHealthCounts(map[string][]string) (*HealthCounts, error)
	GetServiceGroupsHealthCounts(map[string][]string) (*HealthCounts, error)

	GetServicesCount() (int32, error)
	GetServiceGroupsCount() (int32, error)
	GetSupervisorsCount() (int32, error)
	GetDeploymentsCount() (int32, error)

	UpdateTelemetryReported(context.Context, string) error
	GetTelemetry(context.Context) (Telemetry, error)
	GetUniqueServicesFromPostgres(int64, time.Time) (int64, error)

	// Used by our Integration Tests
	EmptyStorage() error
}

const (
	Critical     = "CRITICAL"
	Warning      = "WARNING"
	Unknown      = "UNKNOWN"
	Ok           = "OK"
	Disconnected = "DISCONNECTED"
	None         = "NONE"
)

type UpdateStrategy int

const (
	NoneStrategy UpdateStrategy = iota
	AtOnceStrategy
	RollingStrategy
	UnrecognizedStrategy
)

// Set as a const to share with the tests
const updateStrategyUnrecognizedName = "UNRECOGNIZED"

func (x UpdateStrategy) String() string {
	switch x {
	case NoneStrategy:
		return "NONE"
	case AtOnceStrategy:
		return "AT-ONCE"
	case RollingStrategy:
		return "ROLLING"
	default:
		return updateStrategyUnrecognizedName
	}
}

func HabitatUpdateStrategyToStorageFormat(strategy habitat.UpdateStrategy) UpdateStrategy {
	switch strategy {
	case habitat.UpdateStrategy_AtOnce:
		return AtOnceStrategy
	case habitat.UpdateStrategy_Rolling:
		return RollingStrategy
	default:
		return UnrecognizedStrategy
	}
}

type Service struct {
	ID                  int64
	SupMemberID         string
	Origin              string
	Name                string
	Version             string
	Release             string
	HCStdout            string
	HCStderr            string
	HCExitStatus        int32
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
	Disconnected        bool
	HealthUpdatedAt     time.Time
}

func (s *Service) FullReleaseString() string {
	return fmt.Sprintf("%s/%s/%s/%s",
		s.Origin, s.Name, s.Version, s.Release)
}

type ServiceGroupDisplay struct {
	ID                   string
	Name                 string
	DeploymentID         int32
	Package              string
	Release              string
	HealthStatus         string
	HealthPercentage     int32
	ServicesHealthCounts HealthCounts
	Application          string
	Environment          string
	DisconnectedCount    int32
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
	Total        int32
	Ok           int32
	Warning      int32
	Critical     int32
	Unknown      int32
	Disconnected int32
}

type Telemetry struct {
	ID                      string    `db:"id" json:"id"`
	LastTelemetryReportedAt time.Time `db:"last_telemetry_reported_at" json:"last_telemetry_reported_at"`
	CreatedAt               time.Time `db:"created_at" json:"created_at"`
}
