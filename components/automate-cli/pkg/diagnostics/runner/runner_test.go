package runner_test

import (
	"errors"
	"io"
	"net/http"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	"github.com/chef/automate/components/automate-cli/pkg/diagnostics"
	"github.com/chef/automate/components/automate-cli/pkg/diagnostics/lbrequest"
	"github.com/chef/automate/components/automate-cli/pkg/diagnostics/runner"
)

func TestRunnerHappyPath(t *testing.T) {
	mockTstCtx := NewMockTestContext()
	generateCalled := false
	verifyCalled := false
	cleanupCalled := false

	r := runner.New(
		runner.WithTestContext(mockTstCtx),
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

func TestRunnerSkippedAtRuntime(t *testing.T) {
	mockTstCtx := NewMockTestContext()
	generateCalled := false
	verifyCalled := false
	cleanupCalled := false

	r := runner.New(
		runner.WithTestContext(mockTstCtx),
		runner.WithDiagnostics(
			diagnostics.Diagnostic{
				Name: "test",
				Skip: func(diagnostics.TestContext) (bool, string, error) {
					return true, "skipped", nil
				},
				Generate: func(diagnostics.TestContext) error {
					generateCalled = true
					return nil
				},
				Verify: func(tstContext diagnostics.VerificationTestContext) {
					verifyCalled = true
					require.Equal(tstContext, "good", "good")
				},
				Cleanup: func(diagnostics.TestContext) error {
					cleanupCalled = true
					return nil
				},
			},
		),
	)

	err := r.Run()
	assert.NoError(t, err)
	assert.False(t, generateCalled)
	assert.False(t, verifyCalled)
	assert.False(t, cleanupCalled)
}

func TestRunnerSkippedAtRuntimeBecauseCheckFailed(t *testing.T) {
	mockTstCtx := NewMockTestContext()
	generateCalled := false
	verifyCalled := false
	cleanupCalled := false

	r := runner.New(
		runner.WithTestContext(mockTstCtx),
		runner.WithDiagnostics(
			diagnostics.Diagnostic{
				Name: "test",
				Skip: func(diagnostics.TestContext) (bool, string, error) {
					return false, "", errors.New("boohoo")
				},
				Generate: func(diagnostics.TestContext) error {
					generateCalled = true
					return nil
				},
				Verify: func(tstContext diagnostics.VerificationTestContext) {
					verifyCalled = true
					require.Equal(tstContext, "good", "good")
				},
				Cleanup: func(diagnostics.TestContext) error {
					cleanupCalled = true
					return nil
				},
			},
		),
	)

	err := r.Run()
	assert.Error(t, err)
	assert.False(t, generateCalled)
	assert.False(t, verifyCalled)
	assert.False(t, cleanupCalled)
}

func TestRunnerAllStepsSkippable(t *testing.T) {
	mockTstCtx := NewMockTestContext()
	r := runner.New(
		runner.WithTestContext(mockTstCtx),
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
	mockTstCtx := NewMockTestContext()
	called := false
	r := runner.New(
		runner.WithTestContext(mockTstCtx),
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
	mockTstCtx := NewMockTestContext()
	r := runner.New(
		runner.WithTestContext(mockTstCtx),
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
	mockTstCtx := NewMockTestContext()
	called := false
	r := runner.New(
		runner.WithTestContext(mockTstCtx),
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
	mockTstCtx := NewMockTestContext()
	called := false
	r := runner.New(
		runner.WithTestContext(mockTstCtx),
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
	mockTstCtx := NewMockTestContext()
	fooCalled := false
	anothermatchingCalled := false
	r := runner.New(
		runner.WithTestContext(mockTstCtx),
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
	mockTstCtx := NewMockTestContext()
	bazCalled := false
	r := runner.New(
		runner.WithTestContext(mockTstCtx),
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
	mockTstCtx := NewMockTestContext()
	anothermatchingCalled := false
	r := runner.New(
		runner.WithTestContext(mockTstCtx),
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

type mockTestContext struct{}

func NewMockTestContext() *mockTestContext {
	return &mockTestContext{}
}

func (*mockTestContext) GetValue(key string, value interface{}) error {
	return nil
}

func (*mockTestContext) SetValue(key string, value interface{}) {}

func (*mockTestContext) WriteJSON(writer io.Writer) error {
	return nil
}

func (*mockTestContext) DoLBRequest(path string, opts ...lbrequest.Opts) (*http.Response, error) {
	return nil, nil
}

func (*mockTestContext) PublishViaNATS([][]byte) error {
	return nil
}

func (*mockTestContext) GetOption(key string) *diagnostics.Option {
	return nil
}

func (*mockTestContext) GetVersion() (string, error) {
	return "latest", nil
}

func (*mockTestContext) IsIAMV2() (bool, error) {
	return true, nil
}

func (*mockTestContext) CleanupAdminToken() error {
	return nil
}
