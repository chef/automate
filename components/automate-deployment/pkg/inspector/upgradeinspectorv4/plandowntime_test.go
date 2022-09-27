package upgradeinspectorv4

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
	expected := "1. You have planned downtime\n"
	assert.Equal(t, expected, tw.Output())
}
