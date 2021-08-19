package integration_test

import (
	"fmt"
	"os"
	"testing"
)

var (
	suite = NewSuite()
)

func TestMain(m *testing.M) {
	if err := suite.GlobalSetup(); err != nil {
		fmt.Printf("Test suite setup failed: %s\n", err)
		os.Exit(99)
	}

	// Execute the test suite and record the exit code
	exitCode := m.Run()

	suite.GlobalTeardown()

	// call with result of m.Run()
	os.Exit(exitCode)
}
