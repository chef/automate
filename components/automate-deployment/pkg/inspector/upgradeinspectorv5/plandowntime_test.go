package upgradeinspectorv5

import (
	"testing"

	"github.com/chef/automate/lib/majorupgrade_utils"
	"github.com/stretchr/testify/assert"
)

func TestPlanDownTime(t *testing.T) {
	tw := majorupgrade_utils.NewCustomWriter()
	pd := NewPlannedDownTimeInspection(tw.CliWriter)
	index := 1
	pd.ShowInfo(&index)
	expected := "1. You have scheduled downtime for the duration of the upgrade. (y/n)\n"
	assert.Equal(t, expected, tw.Output())
}
