package systemresource

import (
	"io/fs"

	"github.com/shirou/gopsutil/disk"
)

type MockSystemResourceInfoImpl struct {
	GetNumberOfCPUFunc    func() int
	GetCPUSpeedFunc       func() (float64, error)
	GetMemoryFunc         func() (float64, error)
	GetDiskSpaceInfoFunc  func(string) (disk.UsageStat, error)
	GetDiskPartitionsFunc func(all bool) ([]disk.PartitionStat, error)
	FormatBytesFunc       func(bytes uint64) string
	GetFilePermissionFunc func(string) (fs.FileMode, error)
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

func (mock *MockSystemResourceInfoImpl) GetDiskPartitions(all bool) ([]disk.PartitionStat, error) {
	return mock.GetDiskPartitionsFunc(all)
}

func (mock *MockSystemResourceInfoImpl) FormatBytes(bytes uint64) string {
	return mock.FormatBytesFunc(bytes)
}
func (mock *MockSystemResourceInfoImpl) GetFilePermission(dirPath string) (fs.FileMode, error) {
	return mock.GetFilePermissionFunc(dirPath)
}
