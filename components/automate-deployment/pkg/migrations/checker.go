package migrations

import (
	"context"
	"net/http"
)

// CheckName is the name of the check. The name should be lowercase,
// and not contain spaces, undersores, or any characters that would
// make the shell behave weirdly. Use `-` in place of spaces
type CheckName string

// CheckStatus represents the status of running a check
type CheckStatus struct {
	// If the check passes, success will be true. Otherwise, it will be false
	Success bool
	// Message to display to the user. Should be set for both success and failure
	Message string
	// Remediation represents the remedeation steps the user should perform if
	// the check has failed
	Remediation string
}

// TestProbe is an interface that abstracts all the external entities our
// check needs to probe to determine if its in a passing state or not
type TestProbe interface {
	Ctx() context.Context
	DeployedProducts() []string
	HTTPClient(serviceName string, ssl bool) (*http.Client, error)
}

// Check represents a test we can run against the system
type Check struct {
	Name        CheckName
	Description string
	Run         func(TestProbe) (CheckStatus, error)
}

// A Checker describes the functionality something that can setup and
// run Checks must provide
type Checker interface {
	// ListChecks returns a list of all the valid, configured Checks
	ListChecks() []CheckName
	// RunCheck runs the specified Check
	RunCheck(context.Context, CheckName) (CheckStatus, error)
}
