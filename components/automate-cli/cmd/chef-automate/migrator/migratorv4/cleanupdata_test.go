package migratorv4

import (
	"errors"
	"testing"

	"github.com/chef/automate/lib/io/fileutils"
	"github.com/chef/automate/lib/majorupgrade_utils"
	"github.com/stretchr/testify/assert"
)

func TestRunCleanCmdForFirstTimeWithSuccess(t *testing.T) {
	cw := majorupgrade_utils.NewCustomWriterWithInputs("y", "y")
	mfu := &fileutils.MockFileSystemUtils{
		GetHabRootPathFunc: func() string { return majorupgrade_utils.HAB_DIR },
	}
	mmu := &MockMigratorV4UtilsImpl{
		ReadV4ChecklistFunc:         func(id, path string) (bool, error) { return true, nil },
		IsExternalElasticSearchFunc: func(timeout int64) bool { return false },
		CreateMigrateJsonFunc:       func() error { return nil },
		ExecuteCommandFunc:          func(command string, args []string, workingDir string) error { return nil },
		UpdatePostChecklistFileFunc: func(id, path string) error { return nil },
	}

	cu := NewCleanUp(cw.CliWriter, mmu, mfu, false, false, SPINNER_TEST_DURATION)
	cu.Clean(false)
	expected1 := "Clean up in progres"
	expected2 := `Would you like to clean up the old Elasticsearch data now? (y/n)`
	expected3 := `Clean up successful`

	assert.Contains(t, cw.Output(), expected1)
	assert.Contains(t, cw.Output(), expected2)
	assert.Contains(t, cw.Output(), expected3)
}

func TestRunCleanCmdIfUserAlreadyExecutedWithFailed(t *testing.T) {
	cw := majorupgrade_utils.NewCustomWriterWithInputs("y", "y")
	mfu := &fileutils.MockFileSystemUtils{
		GetHabRootPathFunc: func() string { return majorupgrade_utils.HAB_DIR },
	}
	mmu := &MockMigratorV4UtilsImpl{
		ReadV4ChecklistFunc:         func(id, path string) (bool, error) { return true, nil },
		IsExternalElasticSearchFunc: func(timeout int64) bool { return false },
		ExecuteCommandFunc:          func(command string, args []string, workingDir string) error { return errors.New("failed to update") },
		UpdatePostChecklistFileFunc: func(id, path string) error { return nil },
	}
	cu := NewCleanUp(cw.CliWriter, mmu, mfu, false, false, SPINNER_TEST_DURATION)
	cu.Clean(false)
	expected1 := `Your have already deleted your old Elasticsearch data.
Do you want to perform clean up again? (y/n)
`
	expected2 := `Would you like to clean up the old Elasticsearch data now? (y/n)`
	expected3 := `Clean up failed`
	assert.Contains(t, cw.Output(), expected1)
	assert.Contains(t, cw.Output(), expected2)
	assert.Contains(t, cw.Output(), expected3)
}

func TestRunCleanCmdFailureReadV4ChecklistError(t *testing.T) {
	cw := majorupgrade_utils.NewCustomWriterWithInputs("y", "y")
	mmu := &MockMigratorV4UtilsImpl{
		ReadV4ChecklistFunc:         func(id string, path string) (bool, error) { return false, errors.New("unexpected") },
		IsExternalElasticSearchFunc: func(timeout int64) bool { return false },
		ExecuteCommandFunc:          func(command string, args []string, workingDir string) error { return errors.New("failed to update") },
		UpdatePostChecklistFileFunc: func(id string, path string) error { return nil },
	}
	mfu := &fileutils.MockFileSystemUtils{
		GetHabRootPathFunc: func() string { return "/hab" },
	}

	cu := NewCleanUp(cw.CliWriter, mmu, mfu, false, false, SPINNER_TEST_DURATION)
	err := cu.Clean(false)
	assert.Error(t, err, "unexpected")
}

func TestRunCleanCmdFailureAskForConfirmationFirstPromt(t *testing.T) {
	cw := majorupgrade_utils.NewCustomWriterWithInputs("x")
	mmu := &MockMigratorV4UtilsImpl{
		ReadV4ChecklistFunc:         func(id string, path string) (bool, error) { return false, nil },
		IsExternalElasticSearchFunc: func(timeout int64) bool { return false },
		ExecuteCommandFunc:          func(command string, args []string, workingDir string) error { return nil },
		UpdatePostChecklistFileFunc: func(id string, path string) error { return nil },
	}
	mfu := &fileutils.MockFileSystemUtils{
		GetHabRootPathFunc: func() string { return "/hab" },
	}

	cu := NewCleanUp(cw.CliWriter, mmu, mfu, false, false, SPINNER_TEST_DURATION)
	cu.Clean(false)
	assert.Contains(t, cw.Output(), "I don't understand 'x'. Please type 'y' or 'n'.")
}

func TestRunCleanCmdFailureAskForConfirmationSecoundPromt(t *testing.T) {
	cw := majorupgrade_utils.NewCustomWriterWithInputs("y", "x")
	mmu := &MockMigratorV4UtilsImpl{
		ReadV4ChecklistFunc:         func(id string, path string) (bool, error) { return true, nil },
		IsExternalElasticSearchFunc: func(timeout int64) bool { return false },
		ExecuteCommandFunc:          func(command string, args []string, workingDir string) error { return nil },
		UpdatePostChecklistFileFunc: func(id string, path string) error { return nil },
	}
	mfu := &fileutils.MockFileSystemUtils{
		GetHabRootPathFunc: func() string { return "/hab" },
	}

	cu := NewCleanUp(cw.CliWriter, mmu, mfu, false, false, SPINNER_TEST_DURATION)
	cu.Clean(false)
	assert.Contains(t, cw.Output(), "I don't understand 'x'. Please type 'y' or 'n'.")
}

func TestRunCleanCmdFailureAskForConfirmationUserTerminated(t *testing.T) {
	cw := majorupgrade_utils.NewCustomWriterWithInputs("n")
	mmu := &MockMigratorV4UtilsImpl{
		ReadV4ChecklistFunc:         func(id string, path string) (bool, error) { return false, nil },
		IsExternalElasticSearchFunc: func(timeout int64) bool { return false },
		ExecuteCommandFunc:          func(command string, args []string, workingDir string) error { return nil },
		UpdatePostChecklistFileFunc: func(id string, path string) error { return nil },
	}
	mfu := &fileutils.MockFileSystemUtils{
		GetHabRootPathFunc: func() string { return "/hab" },
	}

	cu := NewCleanUp(cw.CliWriter, mmu, mfu, false, false, SPINNER_TEST_DURATION)
	err := cu.Clean(false)
	assert.Error(t, err, "Cleanup Process Terminated.")
}
