package systemresource

import "github.com/shirou/gopsutil/disk"

type MockSystemResourceInfoImpl struct {
	GetNumberOfCPUFunc   func() int
	GetCPUSpeedFunc      func() (float64, error)
	GetMemoryFunc        func() (float64, error)
	GetDiskSpaceInfoFunc func(string) (disk.UsageStat, error)
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

func (mock *MockSystemResourceInfoImpl) GetDiskSpaceInfo(dirPath string) (disk.UsageStat, error) {
	return mock.GetDiskSpaceInfoFunc(dirPath)
}
