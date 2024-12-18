package upgradeinspectorv5

import (
	"testing"

	"github.com/chef/automate/lib/majorupgrade_utils"
	"github.com/stretchr/testify/assert"
)

func TestReIndexing(t *testing.T) {
	tw := majorupgrade_utils.NewCustomWriter()
	ri := NewReindexingInspection(tw.CliWriter, false)
	index := 1
	ri.ShowInfo(&index)
	expected := "1. You want to do reindexing. (y/n)\n"
	assert.Equal(t, expected, tw.Output())
}
