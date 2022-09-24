package upgradeinspectorv4

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestInspectEnsureStatus(t *testing.T) {
	tw := NewTestWriter()
	ds := NewEnsureStatusInspection(tw.CliWriter, &MockUpgradeV4UtilsImp{
		GetServicesStatusFunc: func() (bool, error) {
			return false, nil
		},
	})
	err := ds.Inspect()
	expected := "Please make sure all services are healthy by running chef-automate status"
	assert.Error(t, err)
	assert.Equal(t, expected, err.Error())
}
