package integration_test

import (
	"os"
	"strconv"
	"testing"
)

// Global variables
var (
	autoDeployedChefServerID       = getEnv("INFRA_SERVER_ID", "local-dev")
	autoDeployedChefOrganizationID = getEnv("INFRA_SERVER_ORG_ID", "test-org")
	totalRecords, _                  = strconv.Atoi(getEnv("TOTAL_RECORDS", "10"))
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

// getEnv fetches the ENV variable with fallback default values. 
func getEnv(key, fallback string) string {
	value := os.Getenv(key)
    if value != "" {
        return value
    }
    return fallback
}
