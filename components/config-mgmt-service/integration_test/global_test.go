//
//  Author:: Salim Afiune <afiune@chef.io>
//  Copyright:: Copyright 2017, Chef Software Inc.
//

package integration_test

import (
	"fmt"
	"os"
	"testing"

	"github.com/gofrs/uuid"
)

var (
	// This suite variable will be available for every single test as long as they
	// belong to the 'integration_test' package.
	suite = NewSuite()
)

// newUUID generates a new UUID and returns it as a string
func newUUID() string {
	return uuid.Must(uuid.NewV4()).String()
}

// TestMain allow us to run a setup before running our tests and also
// teardown everything after we have finished testing. Great place to
// initialize our ES indices. (Check out 'suite_test.go')
//
// => Docs: https://golang.org/pkg/testing/#hdr-Main
func TestMain(m *testing.M) {
	// Global Setup hook: Here is where you can initialize anythings you need
	// for your tests to run, things like; Initialize ES indices, insert
	// nodes or runs, etc.
	if err := suite.GlobalSetup(); err != nil {
		fmt.Printf("Test suite setup failed: %s\n", err)
		os.Exit(99)
	}

	// Execute the test suite and record the exit code
	exitCode := m.Run()

	// Teardown hook: It says it all, this hook should clean documents
	// from ES so that the next test can run on a clean env.
	suite.GlobalTeardown()

	// call with result of m.Run()
	os.Exit(exitCode)
}
