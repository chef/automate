package fileutils

type MockFileSystemUtils struct {
	PathExistsFunc             func(path string) (bool, error)
	IsSymlinkFunc              func(path string) (bool, error)
	CalDirSizeInGBFunc         func(path string) (float64, error)
	CheckSpaceAvailabilityFunc func(dir string, minSpace float64) (bool, error)
	GetFreeSpaceinGBFunc       func(dir string) (float64, error)
	GetHabRootPathFunc         func() string
	WriteToFileFunc            func(filepath string, data []byte) error
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
