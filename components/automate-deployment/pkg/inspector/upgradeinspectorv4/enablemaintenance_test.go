package upgradeinspectorv4

import (
	"errors"
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestInspectDisableMaintenanceAlreadyOn(t *testing.T) {
	tw := NewTestWriter()
	dm := NewDisableMaintenanceInspection(tw.CliWriter, &MockUpgradeV4UtilsImp{
		GetMaintenanceStatusFunc: func(timeout int64) (bool, error) {
			return true, nil
		},
		SetMaintenanceModeFunc: func(timeout int64, status bool) (stdOut, stdErr string, err error) {
			return "", "", nil
		},
	}, 10)
	err := dm.Inspect()
	assert.NoError(t, err)
}

func TestInspectDisableMaintenance(t *testing.T) {
	tw := NewTestWriter()
	dm := NewDisableMaintenanceInspection(tw.CliWriter, &MockUpgradeV4UtilsImp{
		GetMaintenanceStatusFunc: func(timeout int64) (bool, error) {
			return false, nil
		},
		SetMaintenanceModeFunc: func(timeout int64, status bool) (stdOut, stdErr string, err error) {
			return "", "", nil
		},
	}, 10)
	err := dm.Inspect()
	assert.NoError(t, err)
}

func TestInspectDisableMaintenanceError(t *testing.T) {
	tw := NewTestWriter()
	dm := NewDisableMaintenanceInspection(tw.CliWriter, &MockUpgradeV4UtilsImp{
		GetMaintenanceStatusFunc: func(timeout int64) (bool, error) {
			return false, nil
		},
		SetMaintenanceModeFunc: func(timeout int64, status bool) (stdOut, stdErr string, err error) {
			return "", "", errors.New("Unexpected")
		},
	}, 10)
	err := dm.Inspect()
	assert.Error(t, err)
}

func TestInspectDisableMaintenanceRollBackCalled(t *testing.T) {
	tw := NewTestWriter()
	dm := NewDisableMaintenanceInspection(tw.CliWriter, &MockUpgradeV4UtilsImp{
		GetMaintenanceStatusFunc: func(timeout int64) (bool, error) {
			return true, nil
		},
		SetMaintenanceModeFunc: func(timeout int64, status bool) (stdOut, stdErr string, err error) {
			return "", "", nil
		},
	}, 10)
	dm.isExecuted = true
	err := dm.RollBackHandler()
	assert.NoError(t, err)
}
