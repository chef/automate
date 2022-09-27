package migratorV4

import (
	"errors"
	"testing"

	"github.com/chef/automate/lib/majorupgrade_utils"
	"github.com/stretchr/testify/assert"
)

func TestRunMigrationScript(t *testing.T) {
	cw := majorupgrade_utils.NewCustomWriter()
	mmu := &MockMigratorV4UtilsImpl{
		ExecShCommandFunc:  func(script string) error { return nil },
		GetHabRootPathFunc: func(cmd string) string { return "/hab" },
	}
	ms := NewMigrationScript(cw.CliWriter, mmu)
	ms.Run()
	expected1 := "Copying Data"
	expected2 := "✔  Data Copied Successfully"
	assert.Contains(t, cw.Output(), expected1)
	assert.Contains(t, cw.Output(), expected2)
}

func TestRunMigrationScriptExecError(t *testing.T) {
	cw := majorupgrade_utils.NewCustomWriter()
	mmu := &MockMigratorV4UtilsImpl{
		ExecShCommandFunc:  func(script string) error { return errors.New("unexpected") },
		GetHabRootPathFunc: func(cmd string) string { return "/hab" },
	}
	ms := NewMigrationScript(cw.CliWriter, mmu)
	ms.Run()
	ms.ErrorHandler()
	expected1 := "✖  Failed to copy data"
	expected2 := "unexpected"
	assert.Contains(t, cw.Output(), expected1)
	assert.Contains(t, cw.Output(), expected2)
}
