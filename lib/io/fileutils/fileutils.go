package fileutils

import (
	"io"
	"io/ioutil"
	"os"
	"os/exec"
	"path/filepath"
	"strings"

	"github.com/pkg/errors"

	"github.com/chef/automate/lib/platform/sys"
	"github.com/sirupsen/logrus"
)

const (
	HAB_ROOT_CMD string = "HAB_LICENSE=accept-no-persist hab pkg path chef/deployment-service"
	HAB_DIR      string = "/hab"
)

type FileUtils interface {
	PathExists(path string) (bool, error)
	IsSymlink(path string) (bool, error)
	CalDirSizeInGB(path string) (float64, error)
	CheckSpaceAvailability(dir string, minSpace float64) (bool, error)
	GetFreeSpaceinGB(dir string) (float64, error)
	GetHabRootPath() string
	WriteToFile(filepath string, data []byte) error
	ReadFile(filename string) ([]byte, error)
	WriteFile(filepath string, data []byte, perm os.FileMode) error
	CreateTempFile(content string, filename string) (string, error)
	DeleteFile(fileName string) error
}

type FileSystemUtils struct{}

func NewFileSystemUtils() FileUtils {
	return &FileSystemUtils{}
}

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
func (fsu *FileSystemUtils) ReadFile(filename string) ([]byte, error) {
	return ReadFile(filename)
}
func (fsu *FileSystemUtils) WriteFile(filepath string, data []byte, perm os.FileMode) error {
	return WriteFile(filepath, data, perm)
}
func (fsu *FileSystemUtils) CreateTempFile(content string, filename string) (string, error) {
	return CreateTempFile(content, filename)
}
func (fsu *FileSystemUtils) DeleteFile(filePath string) error {
	return DeleteFile(filePath)
}

func (fsu *FileSystemUtils) Rename(sourceFile string, destinationFile string) error {
	return Move(sourceFile, destinationFile)
}

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

func WriteToFile(filepath string, data []byte) error {
	return WriteFile(filepath, data, 0775)
}

func ReadFile(filename string) ([]byte, error) {
	return ioutil.ReadFile(filename) // nosemgrep
}

func WriteFile(filepath string, data []byte, perm os.FileMode) error {
	return ioutil.WriteFile(filepath, data, perm) // nosemgrep
}

func CreateTempFile(content string, filename string) (string, error) {
	tempFile, err := os.CreateTemp("", filename)
	if err != nil {
		return "", errors.Wrap(err, "file creation failed ")
	}
	_, err = tempFile.WriteString((content))
	if err != nil {
		return "", errors.Wrap(err, "writing to a file failed ")
	}
	return tempFile.Name(), nil
}

func DeleteFile(filePath string) error {
	return os.Remove(filePath)
}

// Moves file from current/source directory to destination directory.
// Creates the directory, if not already exists
//
// Example usage:
// err := fileutils.Move("file.txt", "/path/to/my/dir/", "");
// Incase of retriving the old file name, leave renamedFileName as empty
func Move(sourceFile string, destinationFile string) error {
	destinationDir, fileName := filepath.Split(destinationFile)

	if err := os.MkdirAll(destinationDir, os.ModePerm); err != nil {
		return err
	}
	err := os.Rename(sourceFile, destinationFile)
	if err != nil {
		// Checks if the error is of type *os.LinkError
		if linkErr, ok := err.(*os.LinkError); ok {
			// Checks the underlying error code
			if linkErr.Err.Error() == "file exists" {
				// Handles the "file exists" error
				err = os.Remove(destinationFile)
				if err != nil {
					return err
				}
				// Retrys the rename operation
				err = os.Rename(sourceFile, destinationDir+fileName)
				if err != nil {
					return err
				}
			}
		}
		return err
	}
	return nil
}
