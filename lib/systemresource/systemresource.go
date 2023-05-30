package systemresource

import (
	"fmt"
	"runtime"

	"github.com/shirou/gopsutil/cpu"
	"github.com/shirou/gopsutil/disk"
	"github.com/shirou/gopsutil/mem"
)

type SystemResourceInfo interface {
	GetNumberOfCPU() int
	GetCPUSpeed() (float64, error)
	GetMemory() (float64, error)
	GetDiskSpaceInfo(dirPath string) (disk.UsageStat, error)
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

	virtualMemory, err := mem.VirtualMemory()
	if err != nil {
		return 0, err
	}

	totalRAM := float64(virtualMemory.Total) / (1024 * 1024 * 1024)
	return totalRAM, nil
}

func (s *SystemResourceInfoImpl) GetDiskSpaceInfo(dirPath string) (disk.UsageStat, error) {

	usage, err := disk.Usage(dirPath)
	if err != nil {
		return disk.UsageStat{}, err
	}
	return *usage, nil
}
