package runner_test

import (
	"errors"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	"github.com/chef/automate/components/automate-cli/pkg/diagnostics"
	"github.com/chef/automate/components/automate-cli/pkg/diagnostics/runner"
)

func TestRunnerHappyPath(t *testing.T) {
	generateCalled := false
	verifyCalled := false
	cleanupCalled := false

	r := runner.New(
		runner.WithDiagnostics(
			diagnostics.Diagnostic{
				Name: "test",
				Generate: func(tstContext diagnostics.TestContext) error {
					generateCalled = true
					return nil
				},
				Verify: func(tstContext diagnostics.VerificationTestContext) {
					verifyCalled = true
					require.Equal(tstContext, "good", "good")
				},
				Cleanup: func(tstContext diagnostics.TestContext) error {
					cleanupCalled = true
					return nil
				},
			},
		),
	)

	err := r.Run()
	assert.NoError(t, err)
	assert.True(t, generateCalled)
	assert.True(t, verifyCalled)
	assert.True(t, cleanupCalled)
}

func TestRunnerAllStepsSkippable(t *testing.T) {
	r := runner.New(
		runner.WithDiagnostics(
			diagnostics.Diagnostic{
				Name: "test",
			},
		),
	)

	err := r.Run()
	assert.NoError(t, err)
}

func TestRunnerCanRequiredAssertions(t *testing.T) {
	// When someone does something like require.Equal(t, "foo", "bar"),
	// testify calls a FailNow method on a testing context. It is expected
	// that such a call stops execution of the running test somehow. The
	// golang testing framework, and this diagnostic framework both just
	// stop the goroutine

	called := false
	r := runner.New(
		runner.WithDiagnostics(
			diagnostics.Diagnostic{
				Name: "test",
				Verify: func(tstContext diagnostics.VerificationTestContext) {
					called = true
					require.NoError(tstContext, errors.New("Badness happened"))
					require.FailNow(t, "Making it here means the runner did not end the test")
				},
			},
		),
	)

	err := r.Run()
	assert.Error(t, err)
	assert.True(t, called)
}

func TestRunnerSkipsVerifyIfGenerateFails(t *testing.T) {
	r := runner.New(
		runner.WithDiagnostics(
			diagnostics.Diagnostic{
				Name: "test",
				Generate: func(tstContext diagnostics.TestContext) error {
					return errors.New("Badness happened")
				},
				Verify: func(tstContext diagnostics.VerificationTestContext) {
					require.FailNow(t, "Making it here means the runner called verify for a failed Generate")
				},
			},
		),
	)

	err := r.Run()
	assert.Error(t, err)
}

func TestRunnerCallsCleanupIfGenerateFails(t *testing.T) {
	called := false
	r := runner.New(
		runner.WithDiagnostics(
			diagnostics.Diagnostic{
				Name: "test",
				Generate: func(tstContext diagnostics.TestContext) error {
					return errors.New("Badness happened")
				},
				Cleanup: func(tstContext diagnostics.TestContext) error {
					called = true
					return nil
				},
			},
		),
	)

	err := r.Run()
	assert.Error(t, err)
	assert.True(t, called)
}

func TestRunnerCallsCleanupIfVerifyFails(t *testing.T) {
	called := false
	r := runner.New(
		runner.WithDiagnostics(
			diagnostics.Diagnostic{
				Name: "test",
				Generate: func(tstContext diagnostics.TestContext) error {
					return nil
				},
				Verify: func(tstContext diagnostics.VerificationTestContext) {
					require.NoError(tstContext, errors.New("Badness happened"))
				},
				Cleanup: func(tstContext diagnostics.TestContext) error {
					called = true
					return nil
				},
			},
		),
	)

	err := r.Run()
	assert.Error(t, err)
	assert.True(t, called)
}

func TestRunnerTagFiltering(t *testing.T) {
	fooCalled := false
	anothermatchingCalled := false
	r := runner.New(
		runner.WithDiagnostics(
			diagnostics.Diagnostic{
				// Name matches foo tag
				Name: "foo",
				Tags: diagnostics.Tags{"notmatching"},
				Generate: func(tstContext diagnostics.TestContext) error {
					fooCalled = true
					return nil
				},
			},
			diagnostics.Diagnostic{
				// Name matches foo tag
				Name: "anothermatching",
				Tags: diagnostics.Tags{"foo"},
				Generate: func(tstContext diagnostics.TestContext) error {
					anothermatchingCalled = true
					return nil
				},
			},
			diagnostics.Diagnostic{
				// Does not match
				Name: "baz",
				Tags: diagnostics.Tags{"notmatching"},
				Generate: func(tstContext diagnostics.TestContext) error {
					require.FailNow(t, "This diagnostic should not have run because its tag does not match")
					return nil
				},
			},
		),
		runner.WithMatchingTags([]diagnostics.TagFilter{"foo"}),
	)

	err := r.Run()
	assert.NoError(t, err)
	assert.True(t, fooCalled)
	assert.True(t, anothermatchingCalled)
}

func TestRunnerTagFilteringNegations(t *testing.T) {
	bazCalled := false
	r := runner.New(
		runner.WithDiagnostics(
			diagnostics.Diagnostic{
				// Name matches foo tag
				Name: "foo",
				Tags: diagnostics.Tags{"notmatching"},
				Generate: func(tstContext diagnostics.TestContext) error {
					require.FailNow(t, "This diagnostic should not have run because its tag does not match")
					return nil
				},
			},
			diagnostics.Diagnostic{
				// Name matches foo tag
				Name: "anothernotmatching",
				Tags: diagnostics.Tags{"foo"},
				Generate: func(tstContext diagnostics.TestContext) error {
					require.FailNow(t, "This diagnostic should not have run because its tag does not match")
					return nil
				},
			},
			diagnostics.Diagnostic{
				// Does not match
				Name: "baz",
				Tags: diagnostics.Tags{"something"},
				Generate: func(tstContext diagnostics.TestContext) error {
					bazCalled = true
					return nil
				},
			},
		),
		runner.WithMatchingTags([]diagnostics.TagFilter{"~foo"}),
	)

	err := r.Run()
	assert.NoError(t, err)
	assert.True(t, bazCalled)
}

func TestRunnerTagFilteringMultipleFilters(t *testing.T) {
	anothermatchingCalled := false
	r := runner.New(
		runner.WithDiagnostics(
			diagnostics.Diagnostic{
				// Name matches foo tag
				Name: "foo",
				Tags: diagnostics.Tags{"notmatching"},
				Generate: func(tstContext diagnostics.TestContext) error {
					require.FailNow(t, "This diagnostic should not have run because its tag does not match")
					return nil
				},
			},
			diagnostics.Diagnostic{
				// Name matches foo tag
				Name: "anothermatching",
				Tags: diagnostics.Tags{"matching"},
				Generate: func(tstContext diagnostics.TestContext) error {
					anothermatchingCalled = true
					return nil
				},
			},
			diagnostics.Diagnostic{
				// Does not match
				Name: "baz",
				Tags: diagnostics.Tags{"notmatching"},
				Generate: func(tstContext diagnostics.TestContext) error {
					require.FailNow(t, "This diagnostic should not have run because its tag does not match")
					return nil
				},
			},
		),
		runner.WithMatchingTags([]diagnostics.TagFilter{"~foo", "anothermatching"}),
	)

	err := r.Run()
	assert.NoError(t, err)
	assert.True(t, anothermatchingCalled)
}
