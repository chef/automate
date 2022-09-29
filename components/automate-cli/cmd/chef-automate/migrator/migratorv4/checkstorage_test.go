package migratorv4

import (
	"errors"
	"testing"

	"github.com/chef/automate/lib/io/fileutils"
	"github.com/chef/automate/lib/majorupgrade_utils"
	"github.com/stretchr/testify/assert"
)

const INSUFFICIENT_SPACE_MSG = "[Error] Insufficient space. Directory /hab/svc/automate-opensearch/data should have minimum 3.30GB space available.\n"

func TestCheckStorage(t *testing.T) {
	cw := majorupgrade_utils.NewCustomWriter()
	mmu := &MockMigratorV4UtilsImpl{}
	mfu := &fileutils.MockFileSystemUtils{
		CalDirSizeInGBFunc:   func(path string) (float64, error) { return 3.0, nil },
		GetFreeSpaceinGBFunc: func(dir string) (float64, error) { return 5.0, nil },
		GetHabRootPathFunc:   func() string { return majorupgrade_utils.HAB_DIR },
	}
	ms := NewCheckStorage(cw.CliWriter, mmu, mfu)
	err := ms.Run()
	assert.NoError(t, err)
	ms.ErrorHandler()
	assert.Equal(t, cw.Output(), "")
}

func TestCheckStorageLowSpace(t *testing.T) {
	cw := majorupgrade_utils.NewCustomWriter()
	mmu := &MockMigratorV4UtilsImpl{}
	mfu := &fileutils.MockFileSystemUtils{
		CalDirSizeInGBFunc:   func(path string) (float64, error) { return 3.0, nil },
		GetFreeSpaceinGBFunc: func(dir string) (float64, error) { return 2.0, nil },
		GetHabRootPathFunc:   func() string { return majorupgrade_utils.HAB_DIR },
	}
	ms := NewCheckStorage(cw.CliWriter, mmu, mfu)
	err := ms.Run()
	assert.Error(t, err)
	ms.ErrorHandler()
	assert.Equal(t, cw.Output(), INSUFFICIENT_SPACE_MSG)
}

func TestCheckStorageErrorInCalDirFunc(t *testing.T) {
	cw := majorupgrade_utils.NewCustomWriter()
	mmu := &MockMigratorV4UtilsImpl{}
	mfu := &fileutils.MockFileSystemUtils{
		CalDirSizeInGBFunc:   func(path string) (float64, error) { return -1, errors.New("unexpected") },
		GetFreeSpaceinGBFunc: func(dir string) (float64, error) { return 2.0, nil },
		GetHabRootPathFunc:   func() string { return majorupgrade_utils.HAB_DIR },
	}
	ms := NewCheckStorage(cw.CliWriter, mmu, mfu)
	err := ms.Run()
	assert.Contains(t, err.Error(), "unexpected")
	ms.ErrorHandler()
	assert.Equal(t, cw.Output(), "[Error] unexpected\n")
}

func TestCheckStorageErrorInGetFreeSpaceFunc(t *testing.T) {
	cw := majorupgrade_utils.NewCustomWriter()
	mmu := &MockMigratorV4UtilsImpl{}
	mfu := &fileutils.MockFileSystemUtils{
		CalDirSizeInGBFunc:   func(path string) (float64, error) { return 3.0, nil },
		GetFreeSpaceinGBFunc: func(dir string) (float64, error) { return -1, errors.New("unexpected") },
		GetHabRootPathFunc:   func() string { return majorupgrade_utils.HAB_DIR },
	}
	ms := NewCheckStorage(cw.CliWriter, mmu, mfu)
	err := ms.Run()
	assert.Contains(t, err.Error(), "unexpected")
	ms.ErrorHandler()
	assert.Equal(t, cw.Output(), "[Error] unexpected\n")
}

func TestCheckStorageErrorInGetFreeSpaceFuncWithPathDoesntExist(t *testing.T) {
	cw := majorupgrade_utils.NewCustomWriter()
	mmu := &MockMigratorV4UtilsImpl{}
	mfu := &fileutils.MockFileSystemUtils{
		CalDirSizeInGBFunc:   func(path string) (float64, error) { return 3.0, nil },
		GetFreeSpaceinGBFunc: func(dir string) (float64, error) { return -1, errors.New("unexpected") },
		PathExistsFunc:       func(path string) (bool, error) { return false, errors.New("unexpected") },
		GetHabRootPathFunc:   func() string { return majorupgrade_utils.HAB_DIR },
	}
	ms := NewCheckStorage(cw.CliWriter, mmu, mfu)
	err := ms.Run()
	assert.Contains(t, err.Error(), "unexpected")
	ms.ErrorHandler()
	assert.Equal(t, cw.Output(), "[Error] unexpected\n")
}
