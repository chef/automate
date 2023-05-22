package systemresourceservice

import (
	"runtime"
	"testing"

	"github.com/bmizerany/assert"
)

func TestGetNumberOfCPU(t *testing.T) {
	sysOsUtil := NewGetOsAndFileSystemInfo()

	type testCase struct {
		numCpuWant int
	}

	testCases := []testCase{
		{
			numCpuWant: runtime.NumCPU(),
		},
	}

	for _, testCase := range testCases {
		t.Run("", func(t *testing.T) {
			numCpu := sysOsUtil.GetNumberOfCPU()
			assert.Equal(t, testCase.numCpuWant, numCpu)
		})
	}
}

func TestGetCPUSpeed(t *testing.T) {
	// sysOsUtil := NewGetOsAndFileSystemInfo()
	//cpu info file content is required
}

func TestGetMemory(t *testing.T) {
	// sysOsUtil := NewGetOsAndFileSystemInfo()
	// cpuSpeed,err := sysOsUtil.GetCPUSpeed()
}
