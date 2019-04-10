package status

import (
	"fmt"
	"testing"

	"github.com/pkg/errors"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestErrorConstructors(t *testing.T) {
	t.Run("New", func(t *testing.T) {
		err := New(UpgradeError, "something happened")
		require.Error(t, err)

		statusErr, ok := err.(Error)
		require.True(t, ok)
		require.Equal(t, "something happened", statusErr.Error())
		require.Equal(t, "something happened", statusErr.Cause().Error())
		require.Equal(t, UpgradeError, statusErr.ExitCode())
		require.Equal(t, "An issue occurred during the upgrade", statusErr.Description())
	})

	t.Run("Errorf", func(t *testing.T) {
		err := Errorf(UpgradeError, "%s happened", "something")
		require.Error(t, err)

		statusErr, ok := err.(Error)
		require.True(t, ok)
		require.Equal(t, "something happened", statusErr.Error())
		require.Equal(t, "something happened", statusErr.Cause().Error())
		require.Equal(t, UpgradeError, statusErr.ExitCode())
		require.Equal(t, "An issue occurred during the upgrade", statusErr.Description())
	})

	t.Run("Wrap", func(t *testing.T) {
		err := errors.New("cause")
		err = Wrap(err, UpgradeError, "something happened")
		require.Error(t, err)

		statusErr, ok := err.(Error)
		require.True(t, ok)
		require.Equal(t, "something happened: cause", statusErr.Error())
		require.Equal(t, "cause", Cause(statusErr).Error())
		require.Equal(t, UpgradeError, statusErr.ExitCode())
		require.Equal(t, "An issue occurred during the upgrade", statusErr.Description())
	})

	t.Run("Wrapf", func(t *testing.T) {
		happened := "happened"
		err := errors.New("cause")
		err = Wrapf(err, UpgradeError, "something %s", happened)
		require.Error(t, err)

		statusErr, ok := err.(Error)
		require.True(t, ok)
		require.Equal(t, "something happened: cause", statusErr.Error())
		require.Equal(t, "cause", Cause(statusErr).Error())
		require.Equal(t, UpgradeError, statusErr.ExitCode())
		require.Equal(t, "An issue occurred during the upgrade", statusErr.Description())
	})
}

func TestErrorModifiers(t *testing.T) {
	t.Run("Annotate", func(t *testing.T) {
		err := errors.New("cause")
		err = Annotate(err, UpgradeError)
		require.Error(t, err)

		statusErr, ok := err.(Error)
		require.True(t, ok)
		require.Equal(t, "cause", statusErr.Error())
		require.Equal(t, UpgradeError, statusErr.ExitCode())
		require.Equal(t, "An issue occurred during the upgrade", statusErr.Description())
	})

	t.Run("WithRecovery", func(t *testing.T) {
		err := New(UpgradeError, "something happened")
		err = WithRecovery(err, "do this to recover")
		require.Error(t, err)

		statusErr, ok := err.(Error)
		require.True(t, ok)
		require.Equal(t, "do this to recover", statusErr.Recovery())
	})
}

func TestCause(t *testing.T) {
	t.Run("with nil", func(t *testing.T) {
		assert.Equal(t, nil, Cause(nil))
	})

	t.Run("with no cause", func(t *testing.T) {
		assert.Equal(t, "error", Cause(errors.New("error")).Error())
	})

	t.Run("with cause", func(t *testing.T) {
		e := errors.Wrap(errors.New("cause"), "wrapper")
		assert.Equal(t, "cause", Cause(e).Error())
	})
}

func TestRecovery(t *testing.T) {
	t.Run("returns innermost recovery instructions", func(t *testing.T) {
		err := New(ConfigError, "bad key")
		err = WithRecovery(err, "fix config.toml")
		err = Wrap(err, DeployError, "deploy failed")
		err = WithRecovery(err, "run deploy again")

		require.Equal(t, "fix config.toml", Recovery(err))
	})
}

func TestStackTrace(t *testing.T) {
	t.Run("returns stack trace on traceable error", func(t *testing.T) {
		err := Wrap(errors.New("foo"), DeployError, "test")

		// The StackTrace should include our outer function name
		require.Contains(t, fmt.Sprintf("%+v", StackTrace(err)), "status.TestStackTrace")
	})
}

func TestExitCode(t *testing.T) {
	t.Run("returns the oldest status error exit code", func(t *testing.T) {
		err := New(FileAccessError, "io error")
		err = Wrap(err, LicenseError, "could not find license")
		err = Wrap(err, DeployError, "could not deploy")

		require.Equal(t, FileAccessError, ExitCode(err))
	})

	t.Run("returns unknown with a generic error", func(t *testing.T) {
		require.Equal(t, UnknownError, ExitCode(errors.New("foo")))
	})
}

func TestErrorType(t *testing.T) {
	t.Run("returns the innermost status error type", func(t *testing.T) {
		err := New(FileAccessError, "io error")
		err = Wrap(err, LicenseError, "could not find license")
		err = Wrap(err, DeployError, "could not deploy")

		require.Equal(t, "FileAccessError", ErrorType(err))
	})

	t.Run("returns unknown with a generic error", func(t *testing.T) {
		require.Equal(t, "UnknownError", ErrorType(errors.New("foo")))
	})
}

func TestDescription(t *testing.T) {
	t.Run("returns the innermost status error description", func(t *testing.T) {
		err := New(FileAccessError, "io error")
		err = Wrap(err, LicenseError, "could not find license")
		err = Wrap(err, DeployError, "could not deploy")

		require.Equal(t, "Unable to access the file or directory: could not deploy: could not find license: io error", Description(err))
	})

	t.Run("returns generic error cause", func(t *testing.T) {
		require.Equal(t, "foo", Description(errors.New("foo")))
	})
}
