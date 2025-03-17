package upgradeinspectorv5

import (
	"testing"

	"github.com/chef/automate/lib/majorupgrade_utils"
	"github.com/stretchr/testify/assert"
)

func TestExternalPGUpgradeCheck(t *testing.T) {
	tw := majorupgrade_utils.NewCustomWriter()
	ex := NewExternalPGUpgradeCheckInspection(tw.CliWriter, true)
	index := 4
	ex.ShowInfo(&index)
	expected := "4. Upgrade your PostgreSQL 13.5 to 17.0 with the help of your Database Administrator (y/n)\n"
	assert.Equal(t, expected, tw.Output())
}
