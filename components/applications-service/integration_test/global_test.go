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
	"github.com/stretchr/testify/assert"
)

var (
	database = "chef_applications_service"
	suite    = NewSuite(database)
	// TODO @afiune add more apps & envs
	// For now we will use only one application called 'app'
	a = "app"
	e = "test-env"
)

// NewHabServiceMsg returns a new hab service message with the provided args
func NewHabServiceMsg(supID, app, env, group,
	origin, name, version, release, health string) *applications.HabService {
	return NewHabServiceChannelMsg(supID, app, env, group,
		origin, name, version, release, health, "stable", "test")
}

// NewHabServiceMsg returns a new hab service message with the provided args
func NewHabServiceChannelMsg(supID, app, env, group,
	origin, name, version, release, health, channel, site string) *applications.HabService {
	return &applications.HabService{
		SupervisorId: supID,
		Application:  app,
		Environment:  env,
		Group:        group,
		PkgIdent: &applications.PackageIdent{
			Origin:  origin,
			Name:    name,
			Version: version,
			Release: release,
		},
		Status:      applications.ServiceStatus(int32(0)), // @afiune customize it when we use this
		HealthCheck: applications.HealthStatus(HealthCheckStringToInt32(health)),
		Channel:     channel,
		Site:        site,
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
func habServicesMatrix() []*applications.HabService {
	return []*applications.HabService{
		// service_group 1 <-> With a Health Status = 'OK'
		NewHabServiceMsg("sup1", a, e, "default", "core", "redis", "0.1.0", "20190101121212", "OK"),
		NewHabServiceMsg("sup2", a, e, "default", "core", "redis", "0.1.0", "20190101121212", "OK"),
		NewHabServiceMsg("sup3", a, e, "default", "core", "redis", "0.1.0", "20190101121212", "OK"),

		// service_group 2 <-> With a Health Status = 'WARNING'
		NewHabServiceMsg("sup1", a, e, "default", "core", "myapp", "0.1.0", "20190101121212", "WARNING"),
		NewHabServiceMsg("sup2", a, e, "default", "core", "myapp", "0.1.0", "20190101121212", "OK"),
		NewHabServiceMsg("sup3", a, e, "default", "core", "myapp", "0.1.0", "20190101121212", "OK"),

		// service_group 3 <-> With a Health Status = 'CRITICAL'
		NewHabServiceMsg("sup1", a, e, "default", "core", "postgres", "0.1.0", "20190101121212", "OK"),
		NewHabServiceMsg("sup2", a, e, "default", "core", "postgres", "0.1.0", "20190101121212", "UNKNOWN"),
		NewHabServiceMsg("sup3", a, e, "default", "core", "postgres", "0.1.0", "20190101121212", "CRITICAL"),

		// service_group 4 <-> With a Health Status = 'UNKNOWN'
		NewHabServiceMsg("sup4", a, e, "default", "core", "test", "0.1.0", "20190101121212", "UNKNOWN"),
	}
}
