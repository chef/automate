package migratorV4

import (
	"errors"
	"testing"

	"github.com/chef/automate/lib/majorupgrade_utils"
	"github.com/stretchr/testify/assert"
)

func TestAutomateStop(t *testing.T) {
	cw := majorupgrade_utils.NewCustomWriter()
	mmu := &MockMigratorV4UtilsImpl{
		StopAutomateFunc: func() error { return nil },
	}
	as := NewAutomateStop(cw.CliWriter, mmu)
	as.Run()
	expected1 := "Stopping Chef Automate"
	expected2 := "✔  Chef Automate Stopped"
	assert.Contains(t, cw.Output(), expected1)
	assert.Contains(t, cw.Output(), expected2)
}

func TestAutomateStopError(t *testing.T) {
	cw := majorupgrade_utils.NewCustomWriter()
	mmu := &MockMigratorV4UtilsImpl{
		StopAutomateFunc: func() error { return errors.New("unexpected") },
	}
	as := NewAutomateStop(cw.CliWriter, mmu)
	as.Run()
	as.ErrorHandler()
	expected1 := "✖  Failed to stop Chef Automate"
	expected2 := "unexpected"
	assert.Contains(t, cw.Output(), expected1)
	assert.Contains(t, cw.Output(), expected2)
}
