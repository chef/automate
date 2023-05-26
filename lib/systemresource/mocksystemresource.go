package systemresource

type MockSystemResourceInfoImpl struct {
	GetNumberOfCPUFunc     func() int
	GetCPUSpeedFunc        func() (float64, error)
	GetMemoryFunc          func() (float64, error)
	GetCheckPathExistsFunc func(string) (bool, error)
	GetDiskSpaceInfoFunc   func(string) (float64, float64, error)
}

func (mock *MockSystemResourceInfoImpl) GetNumberOfCPU() int {
	return mock.GetNumberOfCPUFunc()
}

func (mock *MockSystemResourceInfoImpl) GetCPUSpeed() (float64, error) {
	return mock.GetCPUSpeedFunc()
}

func (mock *MockSystemResourceInfoImpl) GetMemory() (float64, error) {
	return mock.GetMemoryFunc()
}

func (mock *MockSystemResourceInfoImpl) CheckPathExists(path string) (bool, error) {
	return mock.GetCheckPathExistsFunc(path)
}

func (mock *MockSystemResourceInfoImpl) GetDiskSpaceInfo(dirPath string) (float64, float64, error) {
	return mock.GetDiskSpaceInfoFunc(dirPath)
}
