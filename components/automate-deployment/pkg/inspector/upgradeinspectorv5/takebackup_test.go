package upgradeinspectorv5

import (
	"testing"

	"github.com/chef/automate/lib/majorupgrade_utils"
	"github.com/stretchr/testify/assert"
)

func TestTakeBackup(t *testing.T) {
	tw := majorupgrade_utils.NewCustomWriter()
	tb := NewTakeBackupInspection(tw.CliWriter)
	index := 2
	tb.ShowInfo(&index)
	expected := "2. You have taken a backup by running the command: chef automate backup create. (y/n)\n"
	assert.Equal(t, expected, tw.Output())
}
