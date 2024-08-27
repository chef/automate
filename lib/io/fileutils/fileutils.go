package fileutils

import (
	"bufio"
	"fmt"
	"io"
	"os"
	"os/exec"
	"path/filepath"
	"strconv"
	"strings"

	"github.com/pkg/errors"

	"github.com/chef/automate/lib/platform/sys"
	"github.com/chef/toml"
	"github.com/sirupsen/logrus"
)

const (
	HAB_ROOT_CMD string = "HAB_LICENSE=accept-no-persist hab pkg path chef/deployment-service"
	HAB_DIR      string = "/hab"
	HAB_TMP_DIR  string = "/hab/tmp"
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
	CreateTempFile(content string, filename string, dir string) (string, error)
	DeleteFile(fileName string) error
	Move(sourceFile string, destinationFile string) error
	RemoveFirstLine(filePath string) error
	GetFilePermission(filePath string) (int64, error)
	Stat(name string) (os.FileInfo, error)
	RemoveFile(filename string) error
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
func (fsu *FileSystemUtils) CreateTempFile(content string, filename string, dir string) (string, error) {
	return CreateTempFile(content, filename, dir)
}
func (fsu *FileSystemUtils) DeleteFile(filePath string) error {
	return DeleteFile(filePath)
}

func (fsu *FileSystemUtils) Move(sourceFile string, destinationFile string) error {
	return Move(sourceFile, destinationFile)
}
func (fsu *FileSystemUtils) RemoveFirstLine(filePath string) error {
	return RemoveFirstLine(filePath)
}

func (fsu *FileSystemUtils) GetFilePermission(filePath string) (int64, error) {
	return GetFilePermission(filePath)
}

func (fsu *FileSystemUtils) Stat(name string) (os.FileInfo, error) {
	return os.Stat(name)
}

func (fsu *FileSystemUtils) RemoveFile(filename string) error {
	return os.Remove(filename)
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
	home := os.Getenv("HOME")
	path := filename
	if strings.Index(path, "~") == 0 {
		if home == "" {
			return []byte{}, errors.New("path not found, as $HOME is not defined to resolve '~' in the given path: " + filename)
		}
		path = strings.Replace(path, "~", home, 1)
	}
	return os.ReadFile(path)
}

func WriteFile(filepath string, data []byte, perm os.FileMode) error {
	return os.WriteFile(filepath, data, perm)
}

func CreateTempFile(content string, filename string, dir string) (string, error) {
	tempFile, err := os.CreateTemp(dir, filename)
	if err != nil {
		return "", errors.Wrap(err, "file creation failed ")
	}
	_, err = tempFile.WriteString(content)
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
			// Checks the underlying error code & Handles the "file exists" error
			if linkErr.Err.Error() == "file exists" {
				if err = os.Remove(destinationFile); err != nil {
					return err
				}
				// Retrys the rename operation
				if err = os.Rename(sourceFile, destinationDir+fileName); err != nil {
					return err
				}
			}
		}
		return err
	}
	return nil
}

func RemoveFirstLine(filePath string) error {
	// Open the file for reading
	file, err := os.OpenFile(filePath, os.O_RDWR, 0644)
	if err != nil {
		return err
	}
	defer file.Close()

	// Create a temporary file to write the updated content
	tempFile, err := os.CreateTemp(HAB_TMP_DIR, "temp")
	if err != nil {
		return err
	}
	defer tempFile.Close()

	// Create a scanner to read the file line by line
	scanner := bufio.NewScanner(file)

	// Skip the first line
	scanner.Scan()

	// Write the remaining lines to the temporary file
	for scanner.Scan() {
		line := scanner.Text()
		_, err := tempFile.WriteString(line + "\n")
		if err != nil {
			return err
		}
	}

	// Close the files
	file.Close()
	tempFile.Close()

	// Replace the original file with the temporary file
	if err = os.Rename(tempFile.Name(), filePath); err != nil {
		if strings.Contains(err.Error(), "invalid cross-device link") {
			err = errors.Errorf("%v\n\nPlease change directory to /hab and trigger your command again:\n\nUse: cd /hab\n\n", err)
		}
		return err
	}

	return nil
}

// createTomlFileFromConfig created a toml file where path and struct interface is provided
func CreateTomlFileFromConfig(config interface{}, tomlFile string) (string, error) {
	f, err := os.Create(tomlFile)

	if err != nil {
		// failed to create/open the file
		return "", errors.Wrap(err, "Failed to create/open the file, \n%v")
	}
	if err := toml.NewEncoder(f).Encode(config); err != nil {
		// failed to encode
		return "", errors.Wrap(err, "Failed to encode\n%v")
	}
	if err := f.Close(); err != nil {
		return "", errors.Wrap(err, "Failed to close the file\n%v")
	}

	return tomlFile, nil
}

func GetFilePermission(filePath string) (int64, error) {
	info, err := os.Stat(filePath)
	if err != nil {
		return 0, errors.New("Unable to get the file on the path provided")
	}
	permissions := info.Mode().Perm()
	perms := fmt.Sprintf("%04o", permissions)
	testint, err := strconv.ParseInt(perms, 10, 32)
	if err != nil {
		return 0, errors.New("Error while parsing the file permission")
	}
	return testint, nil
}

// CreateHabTmpDir creates a temporary directory in the hab directory
func CreateHabTmpDir() error {
	if _, err := os.Stat(HAB_TMP_DIR); os.IsNotExist(err) {
		err = os.MkdirAll(HAB_TMP_DIR, os.ModePerm)
		if err != nil {
			return err
		}

		// Change the permissions from 0777 to 1777
		err = os.Chmod(HAB_TMP_DIR, 1777|os.ModeSticky)
		if err != nil {
			return err
		}
	}
	return nil
}
