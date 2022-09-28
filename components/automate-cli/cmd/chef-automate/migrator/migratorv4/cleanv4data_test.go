package migratorV4

import (
	"testing"

	"github.com/chef/automate/lib/majorupgrade_utils"
	"github.com/stretchr/testify/assert"
)

func TestRunCleanCmdForFirstTimeWithSuccess(t *testing.T) {
	cw := majorupgrade_utils.NewCustomWriterWithInputs("y", "y")
	UPGRADE_METADATA = "./test.json"

	mmu := &MockMigratorV4UtilsImpl{
		ReadV4ChecklistFunc:         func(id string) (bool, error) { return true, nil },
		IsExternalElasticSearchFunc: func(timeout int64) bool { return false },
		GetHabRootPathFunc:          func(habrootcmd string) string { return "" },
		CreateMigrateJsonFunc:       func() error { return nil },
	}
	mmu.CreateMigrateJson()

	cu := NewCleanUp(cw.CliWriter, mmu, false, false)
	cu.Run()
	expected1 := "Clean up in progres"
	expected2 := `Your old data will be cleaned-up
		Press y to continue and n to Exit (y/n)`
	expected3 := `Clean up successful`

	assert.Contains(t, cw.Output(), expected1)
	assert.Contains(t, cw.Output(), expected2)
	assert.Contains(t, cw.Output(), expected3)
	mmu.DeleteMigrateJson(UPGRADE_METADATA)
}

func TestRunCleanCmdIfUserAlreadyExecutedWithFailed(t *testing.T) {
	cw := majorupgrade_utils.NewCustomWriterWithInputs("y", "y")
	mmu := &MockMigratorV4UtilsImpl{
		ReadV4ChecklistFunc:         func(id string) (bool, error) { return false, nil },
		IsExternalElasticSearchFunc: func(timeout int64) bool { return false },
		GetHabRootPathFunc:          func(habrootcmd string) string { return "/hab" },
	}
	cu := NewCleanUp(cw.CliWriter, mmu, false, false)
	cu.Run()
	expected1 := `Your have already deleted your old Elasticsearch data.
Do you want to perform clean up again? (y/n)
`
	expected2 := `Your old data will be cleaned-up
		Press y to continue and n to Exit (y/n)`
	expected3 := `Clean up failed`
	assert.Contains(t, cw.Output(), expected1)
	assert.Contains(t, cw.Output(), expected2)
	assert.Contains(t, cw.Output(), expected3)
}

func TestRunCleanCmdIfSuccess(t *testing.T) {
	cw := majorupgrade_utils.NewCustomWriterWithInputs("y", "y")
	mmu := &MockMigratorV4UtilsImpl{
		ReadV4ChecklistFunc:         func(id string) (bool, error) { return false, nil },
		IsExternalElasticSearchFunc: func(timeout int64) bool { return false },
		GetHabRootPathFunc:          func(habrootcmd string) string { return "/hab" },
	}
	cu := NewCleanUp(cw.CliWriter, mmu, false, false)
	cu.Run()
	expected1 := `Your have already deleted your old Elasticsearch data.
Do you want to perform clean up again? (y/n)
`
	expected2 := `Your old data will be cleaned-up
		Press y to continue and n to Exit (y/n)`
	assert.Contains(t, cw.Output(), expected1)
	assert.Contains(t, cw.Output(), expected2)
}
