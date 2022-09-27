package migratorV4

import (
	"testing"

	"github.com/chef/automate/lib/io/fileutils"
	"github.com/chef/automate/lib/majorupgrade_utils"
	"github.com/stretchr/testify/assert"
)

func TestCheckStorage(t *testing.T) {
	cw := majorupgrade_utils.NewCustomWriter()
	mmu := &MockMigratorV4UtilsImpl{}
	mfu := &fileutils.MockFileSystemUtils{
		CalDirSizeInGBFunc:   func(path string) (float64, error) { return 3.0, nil },
		GetFreeSpaceinGBFunc: func(dir string) (float64, error) { return 5.0, nil },
	}
	ms := NewCheckStorage(cw.CliWriter, mmu, mfu)
	err := ms.Run()
	assert.NoError(t, err)
}

func TestCheckStorageError(t *testing.T) {
	cw := majorupgrade_utils.NewCustomWriter()
	mmu := &MockMigratorV4UtilsImpl{}
	mfu := &fileutils.MockFileSystemUtils{
		CalDirSizeInGBFunc:   func(path string) (float64, error) { return 3.0, nil },
		GetFreeSpaceinGBFunc: func(dir string) (float64, error) { return 2.0, nil },
	}
	ms := NewCheckStorage(cw.CliWriter, mmu, mfu)
	err := ms.Run()
	assert.Error(t, err)
}
