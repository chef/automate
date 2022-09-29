package migratorv4

import (
	"errors"
	"testing"

	"github.com/chef/automate/lib/majorupgrade_utils"
	"github.com/stretchr/testify/assert"
)

func TestMaintenanceOn(t *testing.T) {
	cw := majorupgrade_utils.NewCustomWriter()
	mmu := &MockMigratorV4UtilsImpl{
		GetMaintenanceStatusFunc: func(timeout int64) (bool, error) { return true, nil },
		SetMaintenanceModeFunc:   func(timeout int64, status bool) (string, string, error) { return "", "", nil },
	}
	ms := NewEnableMaintenance(cw.CliWriter, mmu, 10, SPINNER_TEST_DURATION)
	err := ms.Run()
	assert.NoError(t, err)
	ms.ErrorHandler()
	assert.Contains(t, cw.Output(), "Maintenance mode is already turned ON")
}

func TestMaintenanceOFF(t *testing.T) {
	cw := majorupgrade_utils.NewCustomWriter()
	mmu := &MockMigratorV4UtilsImpl{
		GetMaintenanceStatusFunc: func(timeout int64) (bool, error) { return true, nil },
		SetMaintenanceModeFunc:   func(timeout int64, status bool) (string, string, error) { return "", "", nil },
	}
	ms := NewEnableMaintenance(cw.CliWriter, mmu, 10, SPINNER_TEST_DURATION)
	ms.isExecuted = true
	err := ms.onSuccess()
	assert.NoError(t, err)
	ms.ErrorHandler()
	assert.Contains(t, cw.Output(), "Maintenance mode turned OFF successfully")
}

func TestMaintenanceOnError(t *testing.T) {
	cw := majorupgrade_utils.NewCustomWriter()
	mmu := &MockMigratorV4UtilsImpl{
		GetMaintenanceStatusFunc: func(timeout int64) (bool, error) { return true, errors.New("maintenance error") },
		SetMaintenanceModeFunc:   func(timeout int64, status bool) (string, string, error) { return "", "", nil },
	}
	ms := NewEnableMaintenance(cw.CliWriter, mmu, 10, SPINNER_TEST_DURATION)
	ms.isExecuted = true
	err := ms.Run()
	assert.Error(t, err)
	ms.ErrorHandler()
	assert.Contains(t, cw.Output(), "✖  Failed to turn maintenance mode ON\n[Error] maintenance error\n")
}

func TestMaintenanceOnSetError(t *testing.T) {
	cw := majorupgrade_utils.NewCustomWriter()
	mmu := &MockMigratorV4UtilsImpl{
		GetMaintenanceStatusFunc: func(timeout int64) (bool, error) { return false, nil },
		SetMaintenanceModeFunc: func(timeout int64, status bool) (string, string, error) {
			return "", "", errors.New("maintenance error")
		},
	}
	ms := NewEnableMaintenance(cw.CliWriter, mmu, 10, SPINNER_TEST_DURATION)
	err := ms.Run()
	assert.Error(t, err)
	ms.ErrorHandler()
	assert.Contains(t, cw.Output(), "✖  Failed to turn maintenance mode ON\n[Error] maintenance error\n")
}
