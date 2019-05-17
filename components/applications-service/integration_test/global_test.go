//
//  Author:: Salim Afiune <afiune@chef.io>
//  Copyright:: Copyright 2019, Chef Software Inc.
//

package integration_test

import (
	"fmt"
	"os"
	"testing"

	"github.com/chef/automate/api/external/applications"
	"github.com/chef/automate/api/external/habitat"
	"github.com/stretchr/testify/assert"
)

var (
	database = "chef_applications_service"
	suite    = NewSuite(database)
	// TODO @afiune add more apps & envs
	// For now we will use only one application called 'app'
	a = "app"
	e = "test-env"
	s = "us"
	c = "stable"
)

// func type to override the HealthCheckEvent message
type MessageOverrides func(*habitat.HealthCheckEvent) error

// TestMain allow us to run a setup before running our tests and also
// teardown everything after we have finished testing.
//
// (Check out 'suite_test.go')
//
// => Docs: https://golang.org/pkg/testing/#hdr-Main
func TestMain(m *testing.M) {
	// Global Setup hook: Here is where you can initialize anythings you need
	// for your tests to run
	suite.GlobalSetup()

	// Execute the test suite and record the exit code
	exitCode := m.Run()

	// Teardown hook: It says it all
	suite.GlobalTeardown()

	// call with result of m.Run()
	os.Exit(exitCode)
}

// A default habitat event
func DefaultHabitatEvent() *habitat.HealthCheckEvent {
	return &habitat.HealthCheckEvent{
		EventMetadata: &habitat.EventMetadata{
			Application: a,
			Environment: e,
			Site:        s,
		},
		ServiceMetadata: &habitat.ServiceMetadata{
			UpdateConfig: &habitat.UpdateConfig{
				Strategy: habitat.UpdateStrategy_AtOnce,
				Channel:  c,
			},
		},
		Result: habitat.HealthCheckResult_Ok,
	}
}

// Creates a new HealthCheckEvent message with the provided overrides
func NewHabitatEvent(overrides ...MessageOverrides) *habitat.HealthCheckEvent {
	// Using the default event to be overwritten
	event := DefaultHabitatEvent()

	for _, f := range overrides {
		err := f(event)
		if err != nil {
			fmt.Printf("Error trying to create habitat event message: %s\n", err)
		}
	}

	return event
}

func withSupervisorId(id string) MessageOverrides {
	return func(msg *habitat.HealthCheckEvent) error {
		msg.EventMetadata.SupervisorId = id
		return nil
	}
}

func withFqdn(fqdn string) MessageOverrides {
	return func(msg *habitat.HealthCheckEvent) error {
		msg.EventMetadata.Fqdn = fqdn
		return nil
	}
}

func withPackageIdent(ident string) MessageOverrides {
	return func(msg *habitat.HealthCheckEvent) error {
		msg.ServiceMetadata.PackageIdent = ident
		return nil
	}
}

func withServiceGroup(group string) MessageOverrides {
	return func(msg *habitat.HealthCheckEvent) error {
		msg.ServiceMetadata.ServiceGroup = group
		return nil
	}
}

func withHealth(health string) MessageOverrides {
	return func(msg *habitat.HealthCheckEvent) error {
		msg.Result = habitat.HealthCheckResult(HealthCheckStringToInt32(health))
		return nil
	}
}

func withSite(site string) MessageOverrides {
	return func(msg *habitat.HealthCheckEvent) error {
		msg.EventMetadata.Site = site
		return nil
	}
}

func withoutUpdateStrategy() MessageOverrides {
	return func(msg *habitat.HealthCheckEvent) error {
		msg.ServiceMetadata.UpdateConfig = nil
		return nil
	}
}

func withStrategyAtOnce(channel string) MessageOverrides {
	return func(msg *habitat.HealthCheckEvent) error {
		msg.ServiceMetadata.UpdateConfig = &habitat.UpdateConfig{
			Strategy: habitat.UpdateStrategy_AtOnce,
			Channel:  channel,
		}
		return nil
	}
}

// HealthCheckStringToInt32 converts a health check string to the respective int32 in proto land
func HealthCheckStringToInt32(health string) int32 {
	switch health {
	case "OK", "ok":
		return int32(0)
	case "WARNING", "warning":
		return int32(1)
	case "CRITICAL", "critical":
		return int32(2)
	case "UNKNOWN", "unknown":
		return int32(3)
	default:
		return int32(4)
	}
}

// assertServiceGroupsEqual verifies that the two provided ServiceGroup Array are equal
func assertServiceGroupsEqual(t *testing.T, expected, actual *applications.ServiceGroups) {

	// If the actual service-groups array is nil,
	// then the expected one should also be nil
	if actual == nil {
		assert.Nil(t, expected)
		return
	}
	// If both arrays of service groups are not equal, return an error
	if assert.Equal(t, len(expected.ServiceGroups), len(actual.ServiceGroups),
		fmt.Sprintf("The service-groups doesn't have the same length. (expected:%d) (actual:%d)",
			len(expected.ServiceGroups), len(actual.ServiceGroups))) {

		for i := range expected.ServiceGroups {
			assert.Equal(t,
				expected.ServiceGroups[i].Name,
				actual.ServiceGroups[i].Name,
				"The service_group name is not the expected one")
			assert.Equal(t,
				expected.ServiceGroups[i].Release,
				actual.ServiceGroups[i].Release,
				"The service_group release is not the expected one")
			assert.Equal(t,
				expected.ServiceGroups[i].HealthPercentage,
				actual.ServiceGroups[i].HealthPercentage,
				"The service_group health percentage is not the expected one")
			assert.Equal(t,
				expected.ServiceGroups[i].Status,
				actual.ServiceGroups[i].Status,
				"The service_group status is not the expected one")
			assert.Equal(t,
				expected.ServiceGroups[i].ServicesHealthCounts,
				actual.ServiceGroups[i].ServicesHealthCounts,
				"The services health counts from the service_group is not the expected one")
		}
	}
}

// Returns a matrix of services equal to the following overall Service Group Health Status:
// {
//   "total": 4,
//   "ok": 1,
//   "warning": 1,
//   "critical": 1,
//   "unknown": 1
// }
// TODO @afiune add more apps & envs
func habServicesMatrix() []*habitat.HealthCheckEvent {
	return []*habitat.HealthCheckEvent{
		// service_group 1 <-> With a Health Status = 'OK'
		NewHabitatEvent(
			withSupervisorId("sup1"),
			withServiceGroup("redis.default"),
			withPackageIdent("core/redis/0.1.0/20190101121212"),
		),
		NewHabitatEvent(
			withSupervisorId("sup2"),
			withServiceGroup("redis.default"),
			withPackageIdent("core/redis/0.1.0/20190101121212"),
		),
		NewHabitatEvent(
			withSupervisorId("sup3"),
			withServiceGroup("redis.default"),
			withPackageIdent("core/redis/0.1.0/20190101121212"),
		),

		// service_group 2 <-> With a Health Status = 'WARNING'
		NewHabitatEvent(
			withSupervisorId("sup1"),
			withServiceGroup("myapp.default"),
			withPackageIdent("core/myapp/0.1.0/20190101121212"),
			withHealth("WARNING"),
		),
		NewHabitatEvent(
			withSupervisorId("sup2"),
			withServiceGroup("myapp.default"),
			withPackageIdent("core/myapp/0.1.0/20190101121212"),
		),
		NewHabitatEvent(
			withSupervisorId("sup3"),
			withServiceGroup("myapp.default"),
			withPackageIdent("core/myapp/0.1.0/20190101121212"),
		),

		// service_group 3 <-> With a Health Status = 'CRITICAL'
		NewHabitatEvent(
			withSupervisorId("sup1"),
			withServiceGroup("postgres.default"),
			withPackageIdent("core/postgres/0.1.0/20190101121212"),
		),
		NewHabitatEvent(
			withSupervisorId("sup2"),
			withServiceGroup("postgres.default"),
			withPackageIdent("core/postgres/0.1.0/20190101121212"),
			withHealth("UNKNOWN"),
		),
		NewHabitatEvent(
			withSupervisorId("sup3"),
			withServiceGroup("postgres.default"),
			withPackageIdent("core/postgres/0.1.0/20190101121212"),
			withHealth("CRITICAL"),
		),

		// service_group 4 <-> With a Health Status = 'UNKNOWN'
		NewHabitatEvent(
			withSupervisorId("sup4"),
			withServiceGroup("test.default"),
			withPackageIdent("core/test/0.1.0/20190101121212"),
			withHealth("UNKNOWN"),
			withoutUpdateStrategy(),
			withSite(""),
		),
	}
}
