package upgradeinspectorv4

import (
	"testing"

	"github.com/chef/automate/lib/majorupgrade_utils"
	"github.com/fatih/color"
	"github.com/stretchr/testify/assert"
)

func TestTakeBackup(t *testing.T) {
	tw := majorupgrade_utils.NewCustomWriter()
	tb := NewTakeBackupInspection(tw.CliWriter)
	index := 2
	tb.ShowInfo(&index)
	expected := "2. You have taken backup by running command: " + color.New(color.Bold).Sprint("chef automate backup create\n")
	assert.Equal(t, expected, tw.Output())
}
