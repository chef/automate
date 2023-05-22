package systemresourceservice

type MockGetOsAndFileSystemInfo struct {
	GetNumberOfCPUFunc     func() int
	GetCPUSpeedFunc        func(string) (float64, error)
	GetMemoryFunc          func() (float64, error)
	GetCheckPathExistsFunc func(string) (bool, error)
	GetDiskSpaceInfoFunc   func(string) (float64, float64, error)
}

func (mock *MockGetOsAndFileSystemInfo) GetNumberOfCPU() int {
	return mock.GetNumberOfCPUFunc()
}

func (mock *MockGetOsAndFileSystemInfo) GetCPUSpeed(file string) (float64, error) {
	return mock.GetCPUSpeedFunc(file)
}

func (mock *MockGetOsAndFileSystemInfo) GetMemory() (float64, error) {
	return mock.GetMemoryFunc()
}

func (mock *MockGetOsAndFileSystemInfo) CheckPathExists(path string) (bool, error) {
	return mock.GetCheckPathExistsFunc(path)
}

func (mock *MockGetOsAndFileSystemInfo) GetDiskSpaceInfo(dirPath string) (float64, float64, error) {
	return mock.GetDiskSpaceInfoFunc(dirPath)
}
