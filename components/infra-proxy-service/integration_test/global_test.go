package integration_test

import (
	"os"
	"testing"
)

// Global variables
var (
	autoDeployedChefServerID       = "local-dev"
	autoDeployedChefOrganizationID = "test-org"
	cFile                          = "/src/components/infra-proxy-service/dev/config"
	// This suite variable will be available for every single test as long as they
	// belong to the 'integration_test' package.
	suite = NewSuite()
)

func TestMain(m *testing.M) {
	// Global Setup hook: Here is where you can initialize anythings you need
	// for your tests to run, things like; Initialize ES indices, insert
	// nodes or runs, etc.
	suite.GlobalSetup()

	// Execute the test suite and record the exit code
	exitCode := m.Run()

	// Teardown hook: It says it all, this hook should clean documents
	// from ES so that the next test can run on a clean env.
	suite.GlobalTeardown()

	// call with result of m.Run()
	os.Exit(exitCode)
}
