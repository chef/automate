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
	// @param (sortField, sortAsc, page, pageSize, filters)
	GetServiceGroups(string, bool, int32, int32, map[string][]string) ([]*ServiceGroupDisplay, error)
	// @param (id)
	ServiceGroupExists(string) (string, bool)
	// @param (filters)
	GetServicesHealthCounts(map[string][]string) (*HealthCounts, error)
	GetServiceGroupsHealthCounts() (*HealthCounts, error)

	// Used by our Integration Tests
	EmptyStorage() error
}

const (
	Critical = "CRITICAL"
	Warning  = "WARNING"
	Unknown  = "UNKNOWN"
	Ok       = "OK"
)

type Service struct {
	ID                  int32  `db:"id"`
	SupMemberID         string `db:"sup_member_id"`
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
	PreviousHealth      string    `db:"previous_health"`
	LastEventOccurredAt time.Time `db:"last_event_occurred_at"`
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

type HealthCounts struct {
	Total    int32
	Ok       int32
	Warning  int32
	Critical int32
	Unknown  int32
}
