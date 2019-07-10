//
//  Author:: Salim Afiune <afiune@chef.io>
//  Copyright:: Copyright 2019, Chef Software Inc.
//

package integration_test

import (
	"fmt"
	"math/rand"
	"os"
	"testing"

	"github.com/chef/automate/api/external/applications"
	"github.com/chef/automate/api/external/habitat"
	"github.com/golang/protobuf/ptypes"
	"github.com/stretchr/testify/assert"

	uuid "github.com/chef/automate/lib/uuid4"
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

const (
	// Used to generate random strings
	letterBytes = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"

	// The maximum time that an ingestion test could wait
	maxWaitTimeMs = 5000 // 5 seconds
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
			OccurredAt:  ptypes.TimestampNow(),
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

// Updates the provided HealthCheckEvent message with the provided overrides
func UpdateHabitatEvent(event *habitat.HealthCheckEvent, overrides ...MessageOverrides) {
	for _, f := range overrides {
		err := f(event)
		if err != nil {
			fmt.Printf("Error trying to create habitat event message: %s\n", err)
		}
	}

	// since we are updating the event with new values, we are actually mocking
	// a new habitat event and therefore we must update the timestamp
	event.EventMetadata.OccurredAt = ptypes.TimestampNow()
}

// Creates a new HealthCheckEvent message with all its fields randomized
func NewHabitatEventRandomized() *habitat.HealthCheckEvent {
	var (
		uuid, _ = uuid.NewV4()
		svcName = RandString(10)
		health  = HealthCheckIntToString(rand.Intn(4))
	)

	return NewHabitatEvent(
		withSupervisorId(uuid.String()),
		withPackageIdent(fmt.Sprintf("core/%s/0.1.0/20190101121212", svcName)),
		withServiceGroup(fmt.Sprintf("%s.default", svcName)),
		withStrategyAtOnce("stable"),
		withApplication("cool-app"),
		withEnvironment("development"),
		withFqdn(fmt.Sprintf("%s.example.com", svcName)),
		withHealth(health),
		withSite("us"),
	)
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

func withStrategyRolling(channel string) MessageOverrides {
	return func(msg *habitat.HealthCheckEvent) error {
		msg.ServiceMetadata.UpdateConfig = &habitat.UpdateConfig{
			Strategy: habitat.UpdateStrategy_Rolling,
			Channel:  channel,
		}
		return nil
	}
}

func withApplication(application string) MessageOverrides {
	return func(msg *habitat.HealthCheckEvent) error {
		msg.EventMetadata.Application = application
		return nil
	}
}

func withEnvironment(environment string) MessageOverrides {
	return func(msg *habitat.HealthCheckEvent) error {
		msg.EventMetadata.Environment = environment
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

// HealthCheckIntToString converts a health check int to a string
func HealthCheckIntToString(health int) string {
	switch health {
	case 0:
		return "OK"
	case 1:
		return "WARNING"
	case 2:
		return "CRITICAL"
	default:
		return "UNKNOWN"
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
				expected.ServiceGroups[i].Package,
				actual.ServiceGroups[i].Package,
				"The service_group package is not the expected one")
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
			assert.Equal(t,
				expected.ServiceGroups[i].Application,
				actual.ServiceGroups[i].Application,
				"The service_group application is not the expected one")
			assert.Equal(t,
				expected.ServiceGroups[i].Environment,
				actual.ServiceGroups[i].Environment,
				"The service_group environment is not the expected one")
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

// RandString generates a random string of the provided length
func RandString(n int) string {
	b := make([]byte, n)
	for i := range b {
		b[i] = letterBytes[rand.Intn(len(letterBytes))]
	}
	return string(b)
}

// Returns 4 service groups with a single service each, the services/service-groups are named
// with s single letter:
// => a.default
// => b.default
// => c.default
// => d.default
func habServicesABCD() []*habitat.HealthCheckEvent {
	return []*habitat.HealthCheckEvent{
		NewHabitatEvent(
			withSupervisorId("sup2"),
			withServiceGroup("a.default"),
			withPackageIdent("core/a/0.1.0/20190101121212"),
			withHealth("UNKNOWN"),
			withApplication("a_app"),
			withEnvironment("a_env"),
		),
		NewHabitatEvent(
			withSupervisorId("sup3"),
			withServiceGroup("b.default"),
			withPackageIdent("core/b/0.1.0/20190101121212"),
			withHealth("OK"),
			withApplication("b_app"),
			withEnvironment("b_env"),
		),
		NewHabitatEvent(
			withSupervisorId("sup4"),
			withServiceGroup("c.default"),
			withPackageIdent("core/c/0.1.0/20190101121212"),
			withHealth("WARNING"),
			withApplication("c_app"),
			withEnvironment("c_env"),
		),
		NewHabitatEvent(
			withSupervisorId("sup5"),
			withServiceGroup("d.default"),
			withPackageIdent("core/d/0.1.0/20190101121212"),
			withHealth("CRITICAL"),
			withApplication("d_app"),
			withEnvironment("d_env"),
		),
	}
}

// @afiune there are cases where the order of the services that are returned are not
// exactly the same, we do care about order but in some degree, for example health check
// status and fqdn, but we don't care about things like order of supervisor_id so we can
// skip that check by not specifying it into the expected response
func assertServicesEqual(t *testing.T, expected, actual []*applications.Service) {
	if assert.Equal(t, len(expected), len(actual), "The number of services are not the same") {
		for i, svc := range expected {
			if len(svc.SupervisorId) != 0 {
				assert.Equal(t,
					svc.SupervisorId,
					actual[i].SupervisorId,
					"The supervisor_id of a service is not the expected one")
			}
			assert.Equal(t,
				svc.Release,
				actual[i].Release,
				"The release of a service is not the expected one")
			assert.Equal(t,
				svc.Group,
				actual[i].Group,
				"The group of a service is not the expected one")
			assert.Equal(t,
				svc.HealthCheck,
				actual[i].HealthCheck,
				"The health_check of a service is not the expected one")
			assert.Equal(t,
				svc.Status,
				actual[i].Status,
				"The status of a service is not the expected one")
			assert.Equal(t,
				svc.Application,
				actual[i].Application,
				"The application of a service is not the expected one")
			assert.Equal(t,
				svc.Environment,
				actual[i].Environment,
				"The environment of a service is not the expected one")
			assert.Equal(t,
				svc.Fqdn,
				actual[i].Fqdn,
				"The fqdn of a service is not the expected one")
			assert.Equal(t,
				svc.Channel,
				actual[i].Channel,
				"The channel of a service is not the expected one")
			assert.Equal(t,
				svc.Site,
				actual[i].Site,
				"The site of a service is not the expected one")
		}
	}
}

func habServicesMatrixAllHealthStatusDifferent() []*habitat.HealthCheckEvent {
	return []*habitat.HealthCheckEvent{
		// service_group 1 <-> With a Health Status = 'OK'
		NewHabitatEvent(
			withSupervisorId("sup1"),
			withServiceGroup("redis.default"),
			withPackageIdent("core/redis/0.1.0/20190101121212"),
			withHealth("OK"),
			withFqdn("myapp-us.example.com"),
		),

		// service_group 2 <-> With a Health Status = 'WARNING'
		NewHabitatEvent(
			withSupervisorId("sup1"),
			withServiceGroup("myapp.default"),
			withPackageIdent("core/myapp/0.1.0/20190101121212"),
			withHealth("WARNING"),
			withFqdn("myapp-us.example.com"),
		),

		// service_group 3 <-> With a Health Status = 'CRITICAL'
		NewHabitatEvent(
			withSupervisorId("sup1"),
			withServiceGroup("postgres.default"),
			withPackageIdent("core/postgres/0.1.0/20190101121212"),
			withHealth("CRITICAL"),
			withFqdn("myapp-us.example.com"),
		),

		// service_group 4 <-> With a Health Status = 'UNKNOWN'
		NewHabitatEvent(
			withSupervisorId("sup2"),
			withServiceGroup("test.default"),
			withPackageIdent("core/test/0.1.0/20190101121212"),
			withHealth("UNKNOWN"),
			withFqdn("test-1.example.com"),
		),
		NewHabitatEvent(
			withSupervisorId("sup3"),
			withServiceGroup("temp.default"),
			withPackageIdent("core/temp/0.1.0/20190101121212"),
			withHealth("UNKNOWN"),
			withFqdn("temp.example.com"),
		),
		NewHabitatEvent(
			withSupervisorId("sup4"),
			withServiceGroup("test.default"),
			withPackageIdent("core/test/0.1.0/20190101121212"),
			withFqdn("test-2.example.com"),
		),
	}
}
