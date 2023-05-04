package verifysystemdcreate_test

import (
	"testing"

	"github.com/chef/automate/components/automate-cli/pkg/verifysystemdcreate"
	"github.com/stretchr/testify/assert"
)

func TestSystemdCreateUtils(t *testing.T) {
	scu := verifysystemdcreate.NewSystemdCreateUtilsImpl()
	t.Run("GetBinaryPath should not give error", func(t *testing.T) {
		_, err := scu.GetBinaryPath()
		assert.NoError(t, err)
	})
	t.Run("ExecuteShellCommand should not give error", func(t *testing.T) {
		err := scu.ExecuteShellCommand("ls", []string{})
		assert.NoError(t, err)
	})
	t.Run("ExecuteShellCommand should not give error", func(t *testing.T) {
		err := scu.CreateDestinationAndCopy("", "")
		assert.Error(t, err)
		assert.Contains(t, "read .: is a directory", err.Error())
	})
	t.Run("ExecuteShellCommand should not give error", func(t *testing.T) {
		err := scu.SystemdRunning()
		assert.Error(t, err)
		assert.Contains(t, "Cannot create automate-verify service since systemd is not present on this machine", err.Error())
	})
}
