package systemresource_test

import (
	"errors"
	"testing"

	"github.com/chef/automate/lib/systemresource"
	"github.com/stretchr/testify/assert"
)

func TestGetNumberOfCPU(t *testing.T) {
	systemresource := systemresource.NewSystemResourceInfoImpl()

	ncpu := systemresource.GetNumberOfCPU()
	assert.NotZero(t, ncpu)
}

func TestGetDiskSpaceInfo(t *testing.T) {
	systemresource := systemresource.NewSystemResourceInfoImpl()

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

		t.Run("Disk space check", func(t *testing.T) {
			usage, err := systemresource.GetDiskSpaceInfo(testCase.path)

			if err != nil {
				assert.Zero(t, usage)
				assert.Zero(t, usage)
			} else {
				assert.NotZero(t, usage)
				assert.NotZero(t, usage)
			}
		})
	}
}

func TestGetCPUSpeed(t *testing.T) {
	systemresource := systemresource.NewSystemResourceInfoImpl()
	cpuSpeed, err := systemresource.GetCPUSpeed()

	if err != nil {
		assert.Zero(t, cpuSpeed)
	} else {
		assert.NotZero(t, cpuSpeed)
	}
}

func TestGetMemory(t *testing.T) {
	systemresource := systemresource.NewSystemResourceInfoImpl()
	memory, err := systemresource.GetMemory()

	if err != nil {
		assert.Zero(t, memory)
	} else {
		assert.NotZero(t, memory)
	}
}
