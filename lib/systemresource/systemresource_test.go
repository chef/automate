package systemresource

import (
	"errors"
	"runtime"
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestGetNumberOfCPU(t *testing.T) {
	systemresource := NewSystemResourceInfoImpl()

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
			numCpu := systemresource.GetNumberOfCPU()
			assert.Equal(t, testCase.numCpuWant, numCpu)
		})
	}
}

func TestGetDiskSpaceInfo(t *testing.T) {
	systemresource := NewSystemResourceInfoImpl()

	testCase := []struct {
		path string
		err  error
	}{
		{
			"./",
			nil,
		},
		{
			"/notexist",
			errors.New("path not exist"),
		},
	}

	for _, testCase := range testCase {
		totalSpace, freeSpace, err := systemresource.GetDiskSpaceInfo(testCase.path)

		if err != nil {
			assert.Zero(t, totalSpace)
			assert.Zero(t, freeSpace)
		} else {
			assert.NotZero(t, totalSpace)
			assert.NotZero(t, freeSpace)
		}
	}
}
