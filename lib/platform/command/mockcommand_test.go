package command_test

import (
	"testing"

	"github.com/pkg/errors"
	"github.com/stretchr/testify/assert"

	"github.com/chef/automate/lib/platform/command"
)

// Just a few basic tests to make sure the mocking isn't completely
// broken. While not impossible, since our MockExecutor will fail the
// tests, to test the error cases will take a bit more work.
func TestExpectations(t *testing.T) {
	t.Run("it can mock all Executor functions", func(t *testing.T) {
		mockExec := command.NewMockExecutor(t)
		mockExec.Expect("Output", command.ExpectedCommand{
			Cmd: "probably-wont-exist",
		}).Return("test-value-1", nil)

		output, err := mockExec.Output("probably-wont-exist")
		assert.NoError(t, err)
		assert.Equal(t, "test-value-1", output)

		mockExec.Expect("CombinedOutput", command.ExpectedCommand{
			Cmd: "probably-wont-exist",
		}).Return("test-value-2", nil)

		output, err = mockExec.CombinedOutput("probably-wont-exist")
		assert.NoError(t, err)
		assert.Equal(t, "test-value-2", output)

		mockExec.Expect("Run", command.ExpectedCommand{
			Cmd: "probably-wont-exist",
		}).Return(nil)
		assert.NoError(t, mockExec.Run("probably-wont-exist"))
		mockExec.AssertAllCalled()
	})
	t.Run("it returns the mocked return output", func(t *testing.T) {
		mockExec := command.NewMockExecutor(t)
		mockExec.Expect("Output", command.ExpectedCommand{
			Cmd: "probably-wont-exist",
		}).Return("test-value-1", nil)

		output, err := mockExec.Output("probably-wont-exist")
		assert.NoError(t, err)
		assert.Equal(t, "test-value-1", output)
		mockExec.AssertAllCalled()
	})
	t.Run("it returns the mocked return errors", func(t *testing.T) {
		mockExec := command.NewMockExecutor(t)
		expectedErr := errors.New("test-err")
		mockExec.Expect("Output", command.ExpectedCommand{
			Cmd: "probably-wont-exist",
		}).Return("test-value-1", expectedErr)

		output, err := mockExec.Output("probably-wont-exist")
		assert.Error(t, err)
		assert.Equal(t, expectedErr, err)
		assert.Equal(t, "test-value-1", output)
		mockExec.AssertAllCalled()
	})
	t.Run("empty expected command matches any command", func(t *testing.T) {
		mockExec := command.NewMockExecutor(t)
		expectedErr := errors.New("test-err")
		mockExec.Expect("Output", command.ExpectedCommand{}).Return("test-value-1", expectedErr)
		output, err := mockExec.Output("probably-wont-exist")
		assert.Error(t, err)
		assert.Equal(t, expectedErr, err)
		assert.Equal(t, "test-value-1", output)
		mockExec.AssertAllCalled()
	})
	t.Run("it matches expectations on args", func(t *testing.T) {
		mockExec := command.NewMockExecutor(t)
		mockExec.Expect("Output", command.ExpectedCommand{
			Args: []string{"test-arg-1"},
			Cmd:  "probably-wont-exist",
		}).Return("test-value-1", nil)
		mockExec.Expect("Output", command.ExpectedCommand{
			Args: []string{"test-arg-2"},
			Cmd:  "probably-wont-exist",
		}).Return("test-value-2", nil)
		output, err := mockExec.Output("probably-wont-exist", command.Args("test-arg-1"))
		assert.NoError(t, err)
		assert.Equal(t, "test-value-1", output)
		output, err = mockExec.Output("probably-wont-exist", command.Args("test-arg-2"))
		assert.NoError(t, err)
		assert.Equal(t, "test-value-2", output)
	})
	t.Run("it matches expectations on env vars", func(t *testing.T) {
		mockExec := command.NewMockExecutor(t)
		mockExec.Expect("Output", command.ExpectedCommand{
			Env: []string{"TEST_ENV_1=foo"},
			Cmd: "probably-wont-exist",
		}).Return("test-value-1", nil)
		mockExec.Expect("Output", command.ExpectedCommand{
			Env: []string{"TEST_ENV_2=foo"},
			Cmd: "probably-wont-exist",
		}).Return("test-value-2", nil)
		output, err := mockExec.Output("probably-wont-exist", command.Envvar("TEST_ENV_1", "foo"))
		assert.NoError(t, err)
		assert.Equal(t, "test-value-1", output)
		output, err = mockExec.Output("probably-wont-exist", command.Envvar("TEST_ENV_2", "foo"))
		assert.NoError(t, err)
		assert.Equal(t, "test-value-2", output)
	})
}
