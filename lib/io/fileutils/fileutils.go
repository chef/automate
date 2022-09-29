package fileutils

import (
	"io"
	"io/ioutil"
	"os"
	"os/exec"
	"strings"

	"github.com/pkg/errors"

	"github.com/chef/automate/lib/platform/sys"
	"github.com/sirupsen/logrus"
)

const (
	HAB_ROOT_CMD string = "HAB_LICENSE=accept-no-persist hab pkg path chef/deployment-service"
	HAB_DIR      string = "/hab"
)

// LogCLose closes the given io.Closer, logging any error.
func LogClose(c io.Closer, log logrus.FieldLogger, msg string) {
	if err := c.Close(); err != nil {
		log.WithError(err).Error(msg)
	}
}

// PathExists returns true if the path exists and false if it doesn't
// exist. An error is returned if an unexpected error occurs.  Callers
// who want behavior similar to Ruby's File.exist? or Rusts'
// path::exists functions can ignore the error.
func PathExists(path string) (bool, error) {
	_, err := os.Stat(path)
	if os.IsNotExist(err) {
		return false, nil
	}

	if err != nil {
		return false, err
	}

	return true, nil
}

// IsSymlink returns true if the given path is a symbolic link and
// false otherwise.  An error is returned on
func IsSymlink(path string) (bool, error) {
	fileInfo, err := os.Lstat(path)
	if err != nil {
		return false, err
	}

	return (fileInfo.Mode() & os.ModeSymlink) == os.ModeSymlink, nil
}

func CalDirSizeInGB(path string) (float64, error) {
	size, err := sys.DirSize(path)
	if err != nil {
		return -1, err
	}
	return float64(size) / (1024 * 1024 * 1024), nil
}

func CheckSpaceAvailability(dir string, minSpace float64) (bool, error) {
	dirSpaceInGB, err := GetFreeSpaceinGB(dir)
	if err != nil {
		return false, err
	}
	if minSpace <= dirSpaceInGB {
		return true, nil
	}
	return false, nil
}

func GetFreeSpaceinGB(dir string) (float64, error) {
	v, err := sys.SpaceAvailForPath(dir)
	if err != nil {
		return -1, errors.Wrap(err, "failed in checking "+dir)
	}
	return float64(v) / (1024 * 1024), nil
}

func WriteToFile(filepath string, data []byte) error {
	return ioutil.WriteFile(filepath, data, 775) // nosemgrep
}

type FileUtils interface {
	PathExists(path string) (bool, error)
	IsSymlink(path string) (bool, error)
	CalDirSizeInGB(path string) (float64, error)
	CheckSpaceAvailability(dir string, minSpace float64) (bool, error)
	GetFreeSpaceinGB(dir string) (float64, error)
	GetHabRootPath() string
	WriteToFile(filepath string, data []byte) error
}

func GetHabRootPath() string {
	out, err := exec.Command("/bin/sh", "-c", HAB_ROOT_CMD).Output()
	if err != nil {
		return HAB_DIR
	}
	pkgPath := string(out)
	habIndex := strings.Index(string(pkgPath), "hab")
	rootHab := pkgPath[0 : habIndex+3]
	if rootHab == "" {
		rootHab = HAB_DIR
	}
	return rootHab
}

type FileSystemUtils struct{}

func (fsu *FileSystemUtils) PathExists(path string) (bool, error) {
	return PathExists(path)
}
func (fsu *FileSystemUtils) IsSymlink(path string) (bool, error) {
	return IsSymlink(path)
}
func (fsu *FileSystemUtils) CalDirSizeInGB(path string) (float64, error) {
	return CalDirSizeInGB(path)
}
func (fsu *FileSystemUtils) CheckSpaceAvailability(dir string, minSpace float64) (bool, error) {
	return CheckSpaceAvailability(dir, minSpace)
}
func (fsu *FileSystemUtils) GetFreeSpaceinGB(dir string) (float64, error) {
	return GetFreeSpaceinGB(dir)
}
func (fsu *FileSystemUtils) GetHabRootPath() string {
	return GetHabRootPath()
}
func (fsu *FileSystemUtils) WriteToFile(filepath string, data []byte) error {
	return WriteToFile(filepath, data)
}
