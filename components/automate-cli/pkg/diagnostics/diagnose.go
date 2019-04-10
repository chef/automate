package diagnostics

import (
	"sync"
)

// RegisteredDiagnostics are a list of diagnostics that are registered
var RegisteredDiagnostics []Diagnostic

var ensureArray sync.Once
var mutex sync.Mutex

// Diagnostic is a structure that describes a test we can run. It will
// first get a chance to generate data in the Generate function. Then,
// it will have chance do any verifications in Verify. Be sure to use
// the testify framework there. The last step it will get to perform
// is any cleanup in Cleanup.
// All steps are optional, only name is required
type Diagnostic struct {
	Name     string
	Tags     Tags
	Generate func(TestContext) error
	Verify   func(VerificationTestContext)
	Cleanup  func(TestContext) error
}

// MatchesFilters determines if a set of TagFilters matches the diagnostic
// negations are applied in such a way that they are AND'd together
// nonnegations are OR'd together
func (d Diagnostic) MatchesFilters(tagFilters []TagFilter) bool {
	// This is quite inefficient. If we ever write enough tests, we
	// can write it better.

	matches := true
	for _, filter := range tagFilters {
		if filter.IsNegate() {
			continue
		}

		if Tag(d.Name) == filter.Tag() || d.Tags.Matches(filter.Tag()) {
			matches = true
			break
		} else {
			matches = false
		}
	}

	for _, filter := range tagFilters {
		if filter.IsNegate() {
			if Tag(d.Name) == filter.Tag() || d.Tags.Matches(filter.Tag()) {
				matches = false
				break
			}
		}
	}

	return matches
}

// RegisterDiagnostic registers a diagnostic into the global list of diagnostics
func RegisterDiagnostic(diagnostic Diagnostic) {
	ensureArray.Do(func() {
		RegisteredDiagnostics = []Diagnostic{}
	})

	mutex.Lock()
	defer mutex.Unlock()
	RegisteredDiagnostics = append(RegisteredDiagnostics, diagnostic)
}
