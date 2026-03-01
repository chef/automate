package upgradeinspectorv5

import (
	"errors"
	"testing"

	"github.com/chef/automate/lib/majorupgrade_utils"
	"github.com/stretchr/testify/assert"
)

func TestInspectEnsureStatus(t *testing.T) {
	tw := majorupgrade_utils.NewCustomWriter()
	ds := NewEnsureStatusInspection(tw.CliWriter, &MockUpgradeV5UtilsImp{
		GetServicesStatusFunc: func() (bool, error) {
			return false, nil
		},
	})
	err := ds.Inspect()
	expected := "Please make sure all services are healthy by running chef-automate status"
	assert.Error(t, err)
	assert.Equal(t, expected, err.Error())
}

func TestInspectEnsureStatusError(t *testing.T) {
	tw := majorupgrade_utils.NewCustomWriter()
	ds := NewEnsureStatusInspection(tw.CliWriter, &MockUpgradeV5UtilsImp{
		GetServicesStatusFunc: func() (bool, error) {
			return false, errors.New("unexpected")
		},
	})
	err := ds.Inspect()
	assert.Error(t, err)
	err = ds.ExitHandler()
	assert.NoError(t, err)
	expected := "[Error] unexpected\n"
	assert.Equal(t, expected, tw.Output())

}
