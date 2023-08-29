package verifysystemdcreate

import (
	"os"
	"os/exec"
	"path/filepath"

	"github.com/chef/automate/components/automate-cli/pkg/status"
	"github.com/chef/automate/components/automate-deployment/pkg/target"
	"github.com/chef/automate/lib/io/fileutils"
)

type SystemdCreateUtils interface {
	GetBinaryPath() (string, error)
	SystemdRunning() error
	CreateDestinationAndCopy(binarySrcPath, binaryDestPath string) error
	ExecuteShellCommand(name string, arg []string) error
	GetEnv() string
}

type SystemdCreateUtilsImpl struct{}

func NewSystemdCreateUtilsImpl() SystemdCreateUtils {
	return &SystemdCreateUtilsImpl{}
}

func (scu *SystemdCreateUtilsImpl) GetBinaryPath() (string, error) {
	binaryPath, err := os.Executable()
	if err != nil {
		return "", status.Wrap(err, status.UnknownError, "Error getting executable path")
	}
	binaryPath, err = filepath.EvalSymlinks(binaryPath)
	if err != nil {
		return "", status.Wrap(err, status.UnknownError, "Error evaluating symlinks in binary path")
	}
	return binaryPath, nil
}

func (scu *SystemdCreateUtilsImpl) SystemdRunning() error {
	usingSystemd, err := target.NewLocalTarget(true).SystemdRunning()
	if err != nil {
		return status.Wrap(err, status.UnknownError, "Error checking systemd present or not")
	}
	if !usingSystemd {
		return status.New(status.UnknownError, "Cannot create automate-verify service since systemd is not present on this machine")
	}
	return nil
}
func (scu *SystemdCreateUtilsImpl) CreateDestinationAndCopy(binarySrcPath, binaryDestPath string) error {
	return fileutils.CreateDestinationAndCopy(binarySrcPath, binaryDestPath)
}

func (scu *SystemdCreateUtilsImpl) ExecuteShellCommand(name string, arg []string) error {
	return exec.Command(name, arg...).Run()
}

func (scu *SystemdCreateUtilsImpl) GetEnv() string {
	return os.Getenv("HOME")
}
