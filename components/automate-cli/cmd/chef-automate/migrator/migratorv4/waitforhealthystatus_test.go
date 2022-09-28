package migratorV4

import (
	"testing"

	"github.com/chef/automate/lib/majorupgrade_utils"
	"github.com/stretchr/testify/assert"
)

func TestWaitForHealthyDeffered(t *testing.T) {
	cw := majorupgrade_utils.NewCustomWriter()
	mmu := &MockMigratorV4UtilsImpl{
		ExecuteCommandFunc: func(command string, args []string, workingDir string) error {
			return nil
		},
	}
	wfh := NewWaitForHealthy(cw.CliWriter, mmu)
	err := wfh.DefferedHandler()
	assert.NoError(t, err)
}
