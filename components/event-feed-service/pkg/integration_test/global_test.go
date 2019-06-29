package integration_test

import (
	"os"
	"testing"

	"github.com/gofrs/uuid"
	log "github.com/sirupsen/logrus"
)

// Global variables
var (
	// The elasticsearch URL comes from the environment variable ELASTICSEARCH_URL
	elasticsearchUrl = os.Getenv("ELASTICSEARCH_URL")

	// Suite variable is available to all tests that
	// belong to the 'integration_test' package

	testSuite = NewSuite(elasticsearchUrl)
)

// NewUUID generates a new UUID and returns it as a string
func newUUID() string {
	return uuid.Must(uuid.NewV4()).String()
}

func GetTestSuite() *Suite {
	return testSuite
}

// TestMain allows us to set up a test fixture before the test run
// and tear down afterward.
//
// => Docs: https://golang.org/pkg/testing/#hdr-Main
func TestMain(m *testing.M) {
	log.Infof("ELASTIC URL is: %s", elasticsearchUrl)
	// Global Setup hook: initialize anything required for the test
	// run, e.g., initializing ES indices, inserting test data, etc.
	testSuite.GlobalSetup()

	// Execute the test suite and record the exit code
	exitCode := m.Run()

	// Teardown hook: clean up any test fixtures created
	// for the test so that tests run independently
	testSuite.GlobalTeardown()

	// call with result of m.Run()
	os.Exit(exitCode)
}
