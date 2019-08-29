//
//  Author:: Salim Afiune <afiune@chef.io>
//  Copyright:: Copyright 2018, Chef Software Inc.
//

package integration_test

import (
	"os"
	"testing"
	"time"

	"github.com/gofrs/uuid"
	"github.com/sirupsen/logrus"
)

// Global variables
var (
	// The elasticsearch URL is coming from the environment variable ELASTICSEARCH_URL
	elasticsearchUrl = os.Getenv("ELASTICSEARCH_URL")
	postgresUrl      = os.Getenv("PG_URL")

	// This suite variable will be available for every single test as long as they
	// belong to the 'integration_test' package.
	suite *Suite

	// The configuration file that the Manager config will use inside the test suite
	cFile = "/tmp/.ingest-service.toml"
)

// newUUID generates a new UUID and returns it as a string
func newUUID() string {
	return uuid.Must(uuid.NewV4()).String()
}

// waitForModificationsToApply is a global functions we can use to wait for
// any modificatin to be applied, the science behind it is that we have a single
// place we can tune the time we need for updates to be applied
func waitForModificationsToApply() {
	// wait for the scheduler to stop
	time.Sleep(100 * time.Millisecond)
}

// TestMain allow us to run a setup before running our tests and also
// teardown everything after we have finished testing. Great place to
// initialize our ES indices. (Check out 'suite_test.go')
//
// => Docs: https://golang.org/pkg/testing/#hdr-Main
func TestMain(m *testing.M) {
	var err error
	suite, err = NewGlobalSuite()
	if err != nil {
		logrus.WithError(err).Error("failed to initialize test suite")
		os.Exit(3)
	}

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
