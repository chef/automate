package fileutils

import "io/fs"

type MockFileSystemUtils struct {
	PathExistsFunc             func(path string) (bool, error)
	IsSymlinkFunc              func(path string) (bool, error)
	CalDirSizeInGBFunc         func(path string) (float64, error)
	CheckSpaceAvailabilityFunc func(dir string, minSpace float64) (bool, error)
	GetFreeSpaceinGBFunc       func(dir string) (float64, error)
	GetHabRootPathFunc         func() string
	WriteToFileFunc            func(filepath string, data []byte) error
	ReadFileFunc               func(filepath string) ([]byte, error)
	WriteFileFunc              func(filepath string, data []byte, perm fs.FileMode) error
}

func (fsu *MockFileSystemUtils) PathExists(path string) (bool, error) {
	return fsu.PathExistsFunc(path)
}
func (fsu *MockFileSystemUtils) IsSymlink(path string) (bool, error) {
	return fsu.IsSymlinkFunc(path)
}
func (fsu *MockFileSystemUtils) CalDirSizeInGB(path string) (float64, error) {
	return fsu.CalDirSizeInGBFunc(path)
}
func (fsu *MockFileSystemUtils) CheckSpaceAvailability(dir string, minSpace float64) (bool, error) {
	return fsu.CheckSpaceAvailabilityFunc(dir, minSpace)
}
func (fsu *MockFileSystemUtils) GetFreeSpaceinGB(dir string) (float64, error) {
	return fsu.GetFreeSpaceinGBFunc(dir)
}
func (fsu *MockFileSystemUtils) GetHabRootPath() string {
	return fsu.GetHabRootPathFunc()
}
func (fsu *MockFileSystemUtils) WriteToFile(filepath string, data []byte) error {
	return fsu.WriteToFileFunc(filepath, data)
}
func (fsu *MockFileSystemUtils) ReadFile(filepath string) ([]byte, error) {
	return fsu.ReadFileFunc(filepath)
}
func (fsu *MockFileSystemUtils) WriteFile(filepath string, data []byte, perm fs.FileMode) error {
	return fsu.WriteFileFunc(filepath, data, perm)
}
