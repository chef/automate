package upgradeinspectorv5

import (
	"testing"

	"github.com/chef/automate/lib/majorupgrade_utils"
	"github.com/stretchr/testify/assert"
)

func TestPostChecklistIntimationCheck(t *testing.T) {
	tw := majorupgrade_utils.NewCustomWriter()
	pc := NewPostChecklistIntimationCheckInspection(tw.CliWriter)
	index := 5
	pc.ShowInfo(&index)
	expected := "5. After this upgrade completes, you will have to run Post upgrade steps to ensure your data is migrated and your Automate is ready for use (y/n)\n"
	assert.Equal(t, expected, tw.Output())
}
