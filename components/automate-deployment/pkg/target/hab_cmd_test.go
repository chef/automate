package target_test

import (
	"context"
	"testing"

	"github.com/pkg/errors"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	"github.com/chef/automate/components/automate-deployment/pkg/habpkg"
	"github.com/chef/automate/components/automate-deployment/pkg/target"
	"github.com/chef/automate/lib/platform/command"
)

func TestInstallPackage(t *testing.T) {
	pkgIdent := "origin/name/0.1.0/20180101010101"
	pkg, err := habpkg.FromString(pkgIdent)
	require.NoError(t, err)
	ctx := context.Background()

	t.Run("it runs hab pkg install with the NOCOLORING and NONINTERACTIVE environment variables", func(t *testing.T) {
		mockExecutor := command.NewMockExecutor(t)
		installer := target.NewHabCmd(mockExecutor, false)

		mockExecutor.Expect("CombinedOutput", command.ExpectedCommand{
			Cmd: "hab",
			Env: []string{"HAB_NOCOLORING=true", "HAB_NONINTERACTIVE=true", "HAB_LICENSE=accept-no-persist"},
			Args: []string{
				"pkg",
				"install",
				pkgIdent,
				"--channel", "current"},
		}).Return("test command output", nil)

		_, err := installer.InstallPackage(ctx, &pkg, "current")
		require.NoError(t, err)
		mockExecutor.AssertAllCalled()
	})
	t.Run("it uses OFFLINE_INSTALL if the installer is initialized in offline mode", func(t *testing.T) {
		mockExecutor := command.NewMockExecutor(t)
		installer := target.NewHabCmd(mockExecutor, true)
		mockExecutor.Expect("CombinedOutput", command.ExpectedCommand{
			Cmd: "hab",
			Env: []string{"HAB_NOCOLORING=true", "HAB_NONINTERACTIVE=true", "HAB_LICENSE=accept-no-persist", "HAB_FEAT_OFFLINE_INSTALL=true"},
			Args: []string{
				"pkg",
				"install",
				pkgIdent,
				"--offline",
				"--channel", "current",
			},
		}).Return("test command output", nil)

		_, err := installer.InstallPackage(ctx, &pkg, "current")
		require.NoError(t, err)
		mockExecutor.AssertAllCalled()
	})
	t.Run("it ignores the channel argument if the channel is the empty string", func(t *testing.T) {
		mockExecutor := command.NewMockExecutor(t)
		installer := target.NewHabCmd(mockExecutor, false)
		mockExecutor.Expect("CombinedOutput", command.ExpectedCommand{
			Cmd: "hab",
			Env: []string{"HAB_NOCOLORING=true", "HAB_NONINTERACTIVE=true", "HAB_LICENSE=accept-no-persist"},
			Args: []string{
				"pkg",
				"install",
				pkgIdent,
			},
		}).Return("test command output", nil)

		_, err := installer.InstallPackage(ctx, &pkg, "")
		require.NoError(t, err)
		mockExecutor.AssertAllCalled()
	})

	t.Run("it returns an error if the underlying Habitat command fails", func(t *testing.T) {
		mockExecutor := command.NewMockExecutor(t)
		installer := target.NewHabCmd(mockExecutor, false)
		mockExecutor.Expect("CombinedOutput", command.ExpectedCommand{
			Cmd: "hab",
			Env: []string{"HAB_NOCOLORING=true", "HAB_NONINTERACTIVE=true", "HAB_LICENSE=accept-no-persist"},
			Args: []string{
				"pkg",
				"install",
				pkgIdent,
			},
		}).Return("test command output", errors.New("test command failure"))

		_, err := installer.InstallPackage(ctx, &pkg, "")
		assert.Error(t, err)
		mockExecutor.AssertAllCalled()
	})

}

func TestIsInstalled(t *testing.T) {
	pkgIdent := "origin/name/0.1.0/20180101010101"
	pkg, err := habpkg.FromString(pkgIdent)
	require.NoError(t, err)
	ctx := context.Background()

	t.Run("it runs hab pkg path with the NOCOLORING and NONINTERACTIVE environment variables", func(t *testing.T) {
		mockExecutor := command.NewMockExecutor(t)
		installer := target.NewHabCmd(mockExecutor, false)

		mockExecutor.Expect("Run", command.ExpectedCommand{
			Cmd:  "hab",
			Env:  []string{"HAB_NOCOLORING=true", "HAB_NONINTERACTIVE=true", "HAB_LICENSE=accept-no-persist"},
			Args: []string{"pkg", "path", pkgIdent},
		}).Return(nil)

		_, err := installer.IsInstalled(ctx, &pkg)
		require.NoError(t, err)
		mockExecutor.AssertAllCalled()
	})

	t.Run("returns true if the command was successful", func(t *testing.T) {
		mockExecutor := command.NewMockExecutor(t)
		installer := target.NewHabCmd(mockExecutor, false)

		mockExecutor.Expect("Run", command.ExpectedCommand{
			Cmd:  "hab",
			Env:  []string{"HAB_NOCOLORING=true", "HAB_NONINTERACTIVE=true", "HAB_LICENSE=accept-no-persist"},
			Args: []string{"pkg", "path", pkgIdent},
		}).Return(nil)

		isInstalled, err := installer.IsInstalled(ctx, &pkg)
		require.NoError(t, err)
		assert.True(t, isInstalled)
		mockExecutor.AssertAllCalled()
	})

	t.Run("returns false with no error if the commad failed", func(t *testing.T) {
		mockExecutor := command.NewMockExecutor(t)
		installer := target.NewHabCmd(mockExecutor, false)

		mockExecutor.Expect("Run", command.ExpectedCommand{
			Cmd:  "hab",
			Env:  []string{"HAB_NOCOLORING=true", "HAB_NONINTERACTIVE=true", "HAB_LICENSE=accept-no-persist"},
			Args: []string{"pkg", "path", pkgIdent},
		}).Return(errors.New("no such package"))

		isInstalled, err := installer.IsInstalled(ctx, &pkg)
		require.NoError(t, err)
		assert.False(t, isInstalled)
		mockExecutor.AssertAllCalled()
	})
}

func TestBinlinkPackage(t *testing.T) {
	pkgIdent := "origin/name/0.1.0/20180101010101"
	pkg, err := habpkg.FromString(pkgIdent)
	require.NoError(t, err)
	ctx := context.Background()

	t.Run("it runs hab pkg binlink with the NOCOLORING and NONINTERACTIVE environment variables", func(t *testing.T) {
		mockExecutor := command.NewMockExecutor(t)
		installer := target.NewHabCmd(mockExecutor, false)

		mockExecutor.Expect("CombinedOutput", command.ExpectedCommand{
			Cmd:  "hab",
			Env:  []string{"HAB_NOCOLORING=true", "HAB_NONINTERACTIVE=true", "HAB_LICENSE=accept-no-persist"},
			Args: []string{"pkg", "binlink", "--force", pkgIdent, "some_exe"},
		}).Return("test command output", nil)

		_, err := installer.BinlinkPackage(ctx, &pkg, "some_exe")
		require.NoError(t, err)
		mockExecutor.AssertAllCalled()
	})

	t.Run("returns errors and output from the underlying command", func(t *testing.T) {
		mockExecutor := command.NewMockExecutor(t)
		installer := target.NewHabCmd(mockExecutor, false)

		mockExecutor.Expect("CombinedOutput", command.ExpectedCommand{
			Cmd:  "hab",
			Env:  []string{"HAB_NOCOLORING=true", "HAB_NONINTERACTIVE=true", "HAB_LICENSE=accept-no-persist"},
			Args: []string{"pkg", "binlink", "--force", pkgIdent, "some_exe"},
		}).Return("test command output", errors.New("test command error"))

		output, err := installer.BinlinkPackage(ctx, &pkg, "some_exe")
		assert.Error(t, err)
		assert.Equal(t, "test command output", output)
		mockExecutor.AssertAllCalled()
	})
}
