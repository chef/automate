package systemresource

import (
	"fmt"
	"runtime"

	"github.com/chef/automate/lib/io/fileutils"
	"github.com/chef/automate/lib/platform/sys"
	"github.com/shirou/gopsutil/cpu"
	"github.com/shirou/gopsutil/disk"
)

type SystemResourceInfo interface {
	GetNumberOfCPU() int
	GetCPUSpeed() (float64, error)
	GetMemory() (float64, error)
	CheckPathExists(string) (bool, error)
	GetDiskSpaceInfo(dirPath string) (float64, float64, error)
}

func NewSystemResourceInfoImpl() *SystemResourceInfoImpl {
	return &SystemResourceInfoImpl{}
}

type SystemResourceInfoImpl struct{}

func (s *SystemResourceInfoImpl) GetNumberOfCPU() int {
	return runtime.NumCPU()
}

func (s *SystemResourceInfoImpl) GetCPUSpeed() (float64, error) {
	info, err := cpu.Info()
	if err != nil {
		return 0, err
	}

	if len(info) == 0 {
		return 0, fmt.Errorf("no CPU information available")
	}

	cpuSpeed := info[0].Mhz
	return cpuSpeed / 1000, nil
}

func (s *SystemResourceInfoImpl) GetMemory() (float64, error) {
	systemMemoryInKB, err := sys.SystemMemoryKB()

	if err != nil {
		return 0, err
	}

	val := float64(systemMemoryInKB) / (1024 * 1024)
	return val, nil
}

func (s *SystemResourceInfoImpl) CheckPathExists(path string) (bool, error) {
	return fileutils.PathExists(path)
}

func (s *SystemResourceInfoImpl) GetDiskSpaceInfo(dirPath string) (float64, float64, error) {

	usage, err := disk.Usage(dirPath)
	if err != nil {
		return 0, 0, err
	}

	totalSpaceInGB := float64(usage.Total) / (1024 * 1024 * 1024)
	totalFreeSpaceInGB := float64(usage.Free) / (1024 * 1024 * 1024)
	return totalSpaceInGB, totalFreeSpaceInGB, nil
}
