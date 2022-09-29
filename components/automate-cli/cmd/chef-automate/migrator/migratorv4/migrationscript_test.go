package migratorv4

import (
	"errors"
	"testing"

	"github.com/chef/automate/lib/io/fileutils"
	"github.com/chef/automate/lib/majorupgrade_utils"
	"github.com/stretchr/testify/assert"
)

func TestRunMigrationScript(t *testing.T) {
	cw := majorupgrade_utils.NewCustomWriter()
	mfu := &fileutils.MockFileSystemUtils{
		GetHabRootPathFunc: func() string { return majorupgrade_utils.HAB_DIR },
	}
	mmu := &MockMigratorV4UtilsImpl{
		ExecuteCommandFunc: func(command string, args []string, workingDir string) error { return nil },
	}
	ms := NewMigrationScript(cw.CliWriter, mmu, mfu, SPINNER_TEST_DURATION)
	ms.Run()
	expected1 := "Copying Data"
	expected2 := "✔  Data Copied Successfully"
	assert.Contains(t, cw.Output(), expected1)
	assert.Contains(t, cw.Output(), expected2)
}

func TestRunMigrationScriptExecError(t *testing.T) {
	cw := majorupgrade_utils.NewCustomWriter()
	mfu := &fileutils.MockFileSystemUtils{
		GetHabRootPathFunc: func() string { return majorupgrade_utils.HAB_DIR },
	}
	mmu := &MockMigratorV4UtilsImpl{
		ExecuteCommandFunc: func(command string, args []string, workingDir string) error { return errors.New("unexpected") },
	}
	ms := NewMigrationScript(cw.CliWriter, mmu, mfu, SPINNER_TEST_DURATION)
	ms.Run()
	ms.ErrorHandler()
	expected1 := "✖  Failed to copy data"
	expected2 := "unexpected"
	assert.Contains(t, cw.Output(), expected1)
	assert.Contains(t, cw.Output(), expected2)
}
