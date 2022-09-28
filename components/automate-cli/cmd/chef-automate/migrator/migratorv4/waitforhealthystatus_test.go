package migratorV4

import (
	"errors"
	"testing"

	"github.com/chef/automate/lib/majorupgrade_utils"
	"github.com/stretchr/testify/assert"
)

func TestWaitForHealthyDeffered(t *testing.T) {
	cw := majorupgrade_utils.NewCustomWriter()
	mmu := &MockMigratorV4UtilsImpl{
		ExecuteCommandFunc: func(command string, args []string, workingDir string) error {
			return nil
		},
	}
	var st = true
	wfh := NewWaitForHealthy(cw.CliWriter, mmu, &st)
	err := wfh.DefferedHandler()
	assert.NoError(t, err)
	expected1 := "  Checking Chef automate status"
	expected2 := "✔  Chef Automate status is healthy"
	assert.Contains(t, cw.Output(), expected1)
	assert.Contains(t, cw.Output(), expected2)
}

func TestWaitForHealthyDefferedError(t *testing.T) {
	cw := majorupgrade_utils.NewCustomWriter()
	mmu := &MockMigratorV4UtilsImpl{
		ExecuteCommandFunc: func(command string, args []string, workingDir string) error {
			return errors.New("terminated due to error")
		},
	}
	var st = true
	wfh := NewWaitForHealthy(cw.CliWriter, mmu, &st)
	err := wfh.DefferedHandler()
	assert.Error(t, err)
	assert.Equal(t, err.Error(), "terminated due to error")
	expected1 := "  Checking Chef automate status"
	expected2 := "✖  Chef Automate status is unhealthy"
	assert.Contains(t, cw.Output(), expected1)
	assert.Contains(t, cw.Output(), expected2)
}
