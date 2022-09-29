package migratorV4

import (
	"errors"
	"testing"

	"github.com/chef/automate/lib/majorupgrade_utils"
	"github.com/stretchr/testify/assert"
)

func TestRunCleanCmdForFirstTimeWithSuccess(t *testing.T) {
	cw := majorupgrade_utils.NewCustomWriterWithInputs("y", "y")

	mmu := &MockMigratorV4UtilsImpl{
		ReadV4ChecklistFunc:         func(id string) (bool, error) { return true, nil },
		IsExternalElasticSearchFunc: func(timeout int64) bool { return false },
		GetHabRootPathFunc:          func(habrootcmd string) string { return "" },
		CreateMigrateJsonFunc:       func() error { return nil },
		ExecuteCommandFunc:          func(command string, args []string, workingDir string) error { return nil },
		UpdatePostChecklistFileFunc: func(id string) error { return nil },
	}

	cu := NewCleanUp(cw.CliWriter, mmu, false, false)
	cu.Clean()
	expected1 := "Clean up in progres"
	expected2 := `Would you like to clean up the old Elasticsearch data now? (y/n)`
	expected3 := `Clean up successful`

	assert.Contains(t, cw.Output(), expected1)
	assert.Contains(t, cw.Output(), expected2)
	assert.Contains(t, cw.Output(), expected3)
}

func TestRunCleanCmdIfUserAlreadyExecutedWithFailed(t *testing.T) {
	cw := majorupgrade_utils.NewCustomWriterWithInputs("y", "y")
	mmu := &MockMigratorV4UtilsImpl{
		ReadV4ChecklistFunc:         func(id string) (bool, error) { return true, nil },
		IsExternalElasticSearchFunc: func(timeout int64) bool { return false },
		GetHabRootPathFunc:          func(habrootcmd string) string { return "/hab" },
		ExecuteCommandFunc:          func(command string, args []string, workingDir string) error { return errors.New("failed to update") },
		UpdatePostChecklistFileFunc: func(id string) error { return nil },
	}
	cu := NewCleanUp(cw.CliWriter, mmu, false, false)
	cu.Clean()
	expected1 := `Your have already deleted your old Elasticsearch data.
Do you want to perform clean up again? (y/n)
`
	expected2 := `Would you like to clean up the old Elasticsearch data now? (y/n)`
	expected3 := `Clean up failed`
	assert.Contains(t, cw.Output(), expected1)
	assert.Contains(t, cw.Output(), expected2)
	assert.Contains(t, cw.Output(), expected3)
}
