package verifysystemdcreate_test

import (
	"errors"
	"fmt"
	"os"
	"testing"

	"github.com/chef/automate/components/automate-cli/pkg/verifysystemdcreate"
	"github.com/chef/automate/lib/majorupgrade_utils"
	"github.com/stretchr/testify/assert"
)

func setupCopy(t *testing.T) (string, string) {
	tmpDir := t.TempDir()
	tmpDir2 := t.TempDir()
	return tmpDir, tmpDir2
}

func TestSystemdServiceCreate(t *testing.T) {
	cw := majorupgrade_utils.NewCustomWriter()
	createDestinationAndCopy := func(binarySrcPath, binaryDestPath string) error {
		return nil
	}
	executeShellCommand := func(name string, arg []string) error {
		if arg[0] == "is-enabled" {
			return errors.New("Failed to get unit file state for automate-verify.service: No such file or directory")
		}
		return nil
	}

	executeShellCommandPassIsEnabled := func(name string, arg []string) error {
		return nil
	}

	executeShellCommandErr := func(name string, arg []string) error {
		if arg[0] == "is-enabled" {
			return errors.New("Failed to get unit file state for automate-verify.service: No such file or directory")
		}
		return errors.New("systemctl command not found")
	}

	executeShellCommandStartErr := func(name string, arg []string) error {
		if arg[0] == "is-enabled" {
			return errors.New("Failed to get unit file state for automate-verify.service: No such file or directory")
		} else if arg[0] == "start" {
			return errors.New("systemctl command not found")
		}
		return nil
	}

	executeShellCommandEnableErr := func(name string, arg []string) error {
		if arg[0] == "is-enabled" {
			return errors.New("Failed to get unit file state for automate-verify.service: No such file or directory")
		} else if arg[0] == "enable" {
			return errors.New("systemctl command not found")
		}
		return nil
	}

	setupMockUtils := func() *verifysystemdcreate.MockSystemdCreateUtils {
		return &verifysystemdcreate.MockSystemdCreateUtils{
			GetBinaryPathFunc:            func() (string, error) { return "", nil },
			SystemdRunningFunc:           func() error { return nil },
			CreateDestinationAndCopyFunc: createDestinationAndCopy,
			ExecuteShellCommandFunc:      executeShellCommand,
			GetEnvFunc:                   func() string { return "/home/ubuntu" },
		}
	}

	t.Run("it gives error if binary destination directory is empty", func(t *testing.T) {
		_, systemdLocation := setupCopy(t)
		_, err := verifysystemdcreate.NewCreateSystemdService(setupMockUtils(), "", systemdLocation, false, cw.CliWriter)
		assert.Error(t, err)
		assert.Equal(t, "Binary destination folder cannot be empty", err.Error())
	})

	t.Run("it gives error if systemd location is empty", func(t *testing.T) {
		binaryDestinationFolder, _ := setupCopy(t)
		_, err := verifysystemdcreate.NewCreateSystemdService(setupMockUtils(), binaryDestinationFolder, "", false, cw.CliWriter)
		assert.Error(t, err)
		assert.Equal(t, "Systemd location cannot be empty", err.Error())
	})

	t.Run("it gives error if systemd create utils is nil", func(t *testing.T) {
		binaryDestinationFolder, _ := setupCopy(t)
		_, err := verifysystemdcreate.NewCreateSystemdService(nil, binaryDestinationFolder, "", false, cw.CliWriter)
		assert.Error(t, err)
		assert.Equal(t, "SystemdCreateUtils cannot be nil", err.Error())
	})

	t.Run("it gives error if writer is nil", func(t *testing.T) {
		binaryDestinationFolder, _ := setupCopy(t)
		_, err := verifysystemdcreate.NewCreateSystemdService(setupMockUtils(), binaryDestinationFolder, "", false, nil)
		assert.Error(t, err)
		assert.Equal(t, "Writer cannot be nil", err.Error())
	})

	t.Run("it creates automate-verify.service file", func(t *testing.T) {
		binaryDestinationFolder, systemdLocation := setupCopy(t)
		vsc, err := verifysystemdcreate.NewCreateSystemdService(setupMockUtils(), binaryDestinationFolder, systemdLocation, false, cw.CliWriter)
		assert.NoError(t, err)
		err = vsc.Create()
		assert.NoError(t, err)
		_, err = os.Stat(systemdLocation + "/automate-verify.service")
		assert.NoError(t, err)
		assert.Contains(t, cw.Output(), "Service automate-verify created successfully")
	})

	t.Run("it restart if there automate-verify systemd-service is already enabled", func(t *testing.T) {
		mockUtils := setupMockUtils()
		mockUtils.ExecuteShellCommandFunc = executeShellCommandPassIsEnabled
		binaryDestinationFolder, systemdLocation := setupCopy(t)
		vsc, err := verifysystemdcreate.NewCreateSystemdService(mockUtils, binaryDestinationFolder, systemdLocation, false, cw.CliWriter)
		assert.NoError(t, err)
		err = vsc.Create()
		assert.NoError(t, err)
		assert.Contains(t, cw.Output(), "Service automate-verify created successfully")
	})

	t.Run("it gives error if there is problem in running shell command", func(t *testing.T) {
		mockUtils := setupMockUtils()
		mockUtils.ExecuteShellCommandFunc = executeShellCommandErr
		binaryDestinationFolder, systemdLocation := setupCopy(t)
		vsc, err := verifysystemdcreate.NewCreateSystemdService(mockUtils, binaryDestinationFolder, systemdLocation, false, cw.CliWriter)
		assert.NoError(t, err)
		err = vsc.Create()
		assert.Error(t, err)
		assert.Equal(t, "Error reloading systemd daemon: systemctl command not found", err.Error())
	})

	t.Run("it gives error if there is problem in running systemctl enable command", func(t *testing.T) {
		mockUtils := setupMockUtils()
		mockUtils.ExecuteShellCommandFunc = executeShellCommandEnableErr
		binaryDestinationFolder, systemdLocation := setupCopy(t)
		vsc, err := verifysystemdcreate.NewCreateSystemdService(mockUtils, binaryDestinationFolder, systemdLocation, false, cw.CliWriter)
		assert.NoError(t, err)
		err = vsc.Create()
		assert.Error(t, err)
		assert.Equal(t, "Error enabling service: systemctl command not found", err.Error())
	})

	t.Run("it gives error if there is problem in running systemctl start command", func(t *testing.T) {
		mockUtils := setupMockUtils()
		mockUtils.ExecuteShellCommandFunc = executeShellCommandStartErr
		binaryDestinationFolder, systemdLocation := setupCopy(t)
		vsc, err := verifysystemdcreate.NewCreateSystemdService(mockUtils, binaryDestinationFolder, systemdLocation, false, cw.CliWriter)
		assert.NoError(t, err)
		err = vsc.Create()
		assert.Error(t, err)
		assert.Equal(t, "Error starting service: systemctl command not found", err.Error())
	})

	t.Run("it gives error if there is problem in createDestinationAndCopy", func(t *testing.T) {
		mockUtils := setupMockUtils()
		mockUtils.ExecuteShellCommandFunc = executeShellCommand
		mockUtils.SystemdRunningFunc = func() error { return errors.New("Error in copying") }
		binaryDestinationFolder, systemdLocation := setupCopy(t)
		vsc, err := verifysystemdcreate.NewCreateSystemdService(mockUtils, binaryDestinationFolder, systemdLocation, false, cw.CliWriter)
		assert.NoError(t, err)
		err = vsc.Create()
		assert.Error(t, err)
		assert.Equal(t, "Error in copying", err.Error())
	})

	t.Run("it overwrites automate-verify.service file if it already exists", func(t *testing.T) {
		mockUtils := setupMockUtils()
		binaryDestinationFolder, systemdLocation := setupCopy(t)
		systemdFilePath := systemdLocation + "/automate-verify.service"
		srcData := []byte("test data")
		assert.NoError(t, os.WriteFile(systemdFilePath, srcData, 0700))
		vsc, err := verifysystemdcreate.NewCreateSystemdService(mockUtils, binaryDestinationFolder, systemdLocation, false, cw.CliWriter)
		assert.NoError(t, err)
		err = vsc.Create()
		assert.NoError(t, err)
		_, err = os.Stat(systemdFilePath)
		assert.NoError(t, err)
		dstData, err := os.ReadFile(systemdFilePath)
		assert.NoError(t, err)
		assert.Contains(t, string(dstData), "Description=Service for automating verification")
	})

	t.Run("it creates automate-verify.service file with proper content inside", func(t *testing.T) {
		mockUtils := setupMockUtils()
		binaryDestinationFolder, systemdLocation := setupCopy(t)
		systemdFilePath := systemdLocation + "/automate-verify.service"
		expectedSystemdContents := `[Unit]
Description=Service for automating verification
After=network.target

[Service]
ExecStart=%s/chef-automate verify serve
Restart=always
StandardOutput=journal
StandardError=journal
Environment="HOME=/home/ubuntu"
[Install]
WantedBy=multi-user.target
`
		vsc, err := verifysystemdcreate.NewCreateSystemdService(mockUtils, binaryDestinationFolder, systemdLocation, false, cw.CliWriter)
		assert.NoError(t, err)
		err = vsc.Create()
		assert.NoError(t, err)
		_, err = os.Stat(systemdFilePath)
		assert.NoError(t, err)
		dstData, err := os.ReadFile(systemdFilePath)
		assert.NoError(t, err)
		assert.Equal(t, fmt.Sprintf(expectedSystemdContents, binaryDestinationFolder), string(dstData))
	})

}
