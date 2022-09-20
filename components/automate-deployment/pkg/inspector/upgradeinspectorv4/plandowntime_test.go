package upgradeinspectorv4

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestPlanDownTime(t *testing.T) {
	tw := NewTestWriter()
	pd := NewPlannedDownTimeInspection(tw.CliWriter)
	index := 1
	pd.ShowInfo(&index)
	expected := "1. You have planned downtime\n"
	assert.Equal(t, expected, tw.Output())
}
