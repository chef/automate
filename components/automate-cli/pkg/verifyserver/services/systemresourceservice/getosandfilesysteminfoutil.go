package systemresourceservice

import (
	"errors"
	"os/exec"
	"runtime"
	"strconv"
	"strings"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/utils/getosutils"
	"github.com/chef/automate/lib/io/fileutils"
	"github.com/chef/automate/lib/platform/sys"
)

const (
	DISK_SPACE_CALC_ERR = "error occured while fetching total and/or free disk space"
)

type IGetOsAndFileSystemInfo interface {
	GetNumberOfCPU() int
	GetCPUSpeed(string) (float64, error)
	GetMemory() (float64, error)
	CheckPathExists(string) (bool, error)
	GetDiskSpaceInfo(dirPath string) (float64, float64, error)
}

func NewGetOsAndFileSystemInfo() *GetOsAndFileSystemInfo {
	return &GetOsAndFileSystemInfo{}
}

type GetOsAndFileSystemInfo struct{}

func (s *GetOsAndFileSystemInfo) GetNumberOfCPU() int {
	return runtime.NumCPU()
}

func (s *GetOsAndFileSystemInfo) GetCPUSpeed(cpuInfoFile string) (float64, error) {
	return getosutils.GetCPUSpeed(cpuInfoFile)
}

func (s *GetOsAndFileSystemInfo) GetMemory() (float64, error) {
	systemMemoryInKB, err := sys.SystemMemoryKB()

	if err != nil {
		return 0, err
	}

	return float64(systemMemoryInKB) / (1024 * 1024), nil
}

func (s *GetOsAndFileSystemInfo) CheckPathExists(path string) (bool, error) {
	return fileutils.PathExists(path)
}

// parsing output of df command.
func (s *GetOsAndFileSystemInfo) GetDiskSpaceInfo(dirPath string) (float64, float64, error) {

	cmd := exec.Command("df", "-k", dirPath)
	output, err := cmd.Output()
	if err != nil {
		return 0, 0, errors.New(DISK_SPACE_CALC_ERR)
	}

	lines := strings.Split(string(output), "\n")
	if len(lines) < 2 {
		return 0, 0, errors.New(DISK_SPACE_CALC_ERR)
	}

	fields := strings.Fields(lines[1])
	if len(fields) < 4 {
		return 0, 0, errors.New(DISK_SPACE_CALC_ERR)
	}

	totalSpaceKB, err := strconv.ParseFloat(fields[1], 64)
	if err != nil {
		return 0, 0, errors.New(DISK_SPACE_CALC_ERR)
	}

	totalFreeSpaceKB, err := strconv.ParseFloat(fields[3], 64)
	if err != nil {
		return 0, 0, errors.New(DISK_SPACE_CALC_ERR)
	}

	totalSpaceInGB := float64(totalSpaceKB) / (1024 * 1024)
	totalFreeSpaceInGB := float64(totalFreeSpaceKB) / (1024 * 1024)

	return totalSpaceInGB, totalFreeSpaceInGB, nil
}
