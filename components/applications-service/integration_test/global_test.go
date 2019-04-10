//
//  Author:: Salim Afiune <afiune@chef.io>
//  Copyright:: Copyright 2019, Chef Software Inc.
//

package integration_test

import (
	"os"
	"testing"

	"github.com/chef/automate/api/external/applications"
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
