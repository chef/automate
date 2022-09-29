package migratorv4

import (
	"errors"
	"testing"

	"github.com/chef/automate/lib/io/fileutils"
	"github.com/chef/automate/lib/majorupgrade_utils"
	"github.com/stretchr/testify/assert"
)

func TestDirectoryAvailability(t *testing.T) {
	cw := majorupgrade_utils.NewCustomWriter()
	mfu := &fileutils.MockFileSystemUtils{
		PathExistsFunc: func(path string) (bool, error) {
			return true, nil
		},
	}
	ms := NewCheckDirExists(cw.CliWriter, mfu)
	err := ms.Run()
	assert.NoError(t, err)
	ms.ErrorHandler()
	assert.Equal(t, cw.Output(), "")
}

func TestDirectoryAvailabilityError(t *testing.T) {
	cw := majorupgrade_utils.NewCustomWriter()
	mfu := &fileutils.MockFileSystemUtils{
		PathExistsFunc: func(path string) (bool, error) {
			return false, errors.New("permission error")
		},
	}
	ms := NewCheckDirExists(cw.CliWriter, mfu)
	err := ms.Run()
	assert.Error(t, err)
	ms.ErrorHandler()
	assert.Equal(t, cw.Output(), "[Error] Failed to check directory /hab/svc/automate-elasticsearch/data: permission error\n")
}

func TestDirectoryAvailabilityOneNotFound(t *testing.T) {
	cw := majorupgrade_utils.NewCustomWriter()
	c := 0
	mfu := &fileutils.MockFileSystemUtils{
		PathExistsFunc: func(path string) (bool, error) {
			if c == 0 {
				c++
				return true, nil
			}
			return false, nil
		},
	}
	ms := NewCheckDirExists(cw.CliWriter, mfu)
	err := ms.Run()
	assert.Error(t, err)
	ms.ErrorHandler()
	assert.Equal(t, cw.Output(), "[Error] Directory not found : /hab/svc/automate-elasticsearch/var\n")
}

func TestDirectoryAvailabilityNotExistsBoth(t *testing.T) {
	cw := majorupgrade_utils.NewCustomWriter()

	mfu := &fileutils.MockFileSystemUtils{
		PathExistsFunc: func(path string) (bool, error) {
			return false, nil
		},
	}
	ms := NewCheckDirExists(cw.CliWriter, mfu)
	err := ms.Run()
	assert.Error(t, err)
	ms.ErrorHandler()
	assert.Equal(t, cw.Output(), "[Error] Directories not found :\n/hab/svc/automate-elasticsearch/data\n/hab/svc/automate-elasticsearch/var\n")
}
