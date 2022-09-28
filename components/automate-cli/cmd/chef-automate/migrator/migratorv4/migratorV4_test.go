package migratorV4

import (
	"errors"
	"testing"

	"github.com/chef/automate/lib/io/fileutils"
	"github.com/chef/automate/lib/majorupgrade_utils"
	"github.com/stretchr/testify/assert"
)

const WISH_TO_MIGRATE = "Do you wish to migrate the Elasticsearch data to OpenSearch now? (y/n)\n"

func TestRunMigrations(t *testing.T) {
	w := majorupgrade_utils.NewCustomWriterWithInputs("y")
	mmu := &MockMigratorV4UtilsImpl{}
	migratorv4 := NewMigratorV4(w.CliWriter, false, false, mmu, &fileutils.MockFileSystemUtils{}, 10)
	migratorv4.(*MigratorV4).AddDefaultMigrationSteps()
	migratorv4.AskForConfirmation()
	t.Log(w.Output())
	assert.Equal(t, w.Output(), WISH_TO_MIGRATE)
}

func TestRunMigrationsWronginput(t *testing.T) {
	w := majorupgrade_utils.NewCustomWriterWithInputs("x")
	mmu := &MockMigratorV4UtilsImpl{}
	migratorv4 := NewMigratorV4(w.CliWriter, false, false, mmu, &fileutils.MockFileSystemUtils{}, 10)
	migratorv4.(*MigratorV4).AddDefaultMigrationSteps()
	migratorv4.AskForConfirmation()
	t.Log(w.Output())
	expected := `Do you wish to migrate the Elasticsearch data to OpenSearch now? (y/n)
I don't understand 'x'. Please type 'y' or 'n'.
Do you wish to migrate the Elasticsearch data to OpenSearch now? (y/n)
`
	assert.Equal(t, w.Output(), expected)
}

func TestRunSuccessfulMigrations(t *testing.T) {
	w := majorupgrade_utils.NewCustomWriterWithInputs("y")
	mmu := &MockMigratorV4UtilsImpl{
		StopAutomateFunc:            func() error { return nil },
		GetEsTotalShardSettingsFunc: func() (int32, error) { return 2000, nil },
		PatchOpensearchConfigFunc:   func(es *ESSettings) (string, string, error) { return "", "", nil },
		GetHabRootPathFunc:          func(habrootcmd string) string { return "/hab" },
		ExecuteCommandFunc:          func(command string, args []string, workingDir string) error { return nil },
		GetServicesStatusFunc:       func() (bool, error) { return true, nil },
	}
	migratorv4 := NewMigratorV4(w.CliWriter, false, false, mmu, &fileutils.MockFileSystemUtils{
		CalDirSizeInGBFunc:   func(path string) (float64, error) { return 3, nil },
		GetFreeSpaceinGBFunc: func(dir string) (float64, error) { return 4, nil },
		PathExistsFunc:       func(path string) (bool, error) { return true, nil },
	}, 10)
	migratorv4.(*MigratorV4).AddDefaultMigrationSteps()
	migratorv4.AskForConfirmation()
	migratorv4.ExecuteMigrationSteps()
	expected1 := "Stopping Chef Automate"
	expected2 := "✔  Chef Automate Stopped"
	expected3 := "Updating OpenSearch configurations"
	expected4 := "✔  OpenSearch configurations updated successfully"
	expected5 := "Copying Data"
	expected6 := "✔  Data Copied Successfully"

	assert.Contains(t, w.Output(), WISH_TO_MIGRATE)
	assert.Contains(t, w.Output(), expected1)
	assert.Contains(t, w.Output(), expected2)
	assert.Contains(t, w.Output(), expected3)
	assert.Contains(t, w.Output(), expected4)
	assert.Contains(t, w.Output(), expected5)
	assert.Contains(t, w.Output(), expected6)

}

func TestRunSuccessfulMigrationsWithError(t *testing.T) {
	w := majorupgrade_utils.NewCustomWriterWithInputs("y")
	mmu := &MockMigratorV4UtilsImpl{
		StopAutomateFunc:            func() error { return nil },
		GetEsTotalShardSettingsFunc: func() (int32, error) { return 2000, nil },
		PatchOpensearchConfigFunc:   func(es *ESSettings) (string, string, error) { return "", "", nil },
		GetHabRootPathFunc:          func(habrootcmd string) string { return "/hab" },
		ExecuteCommandFunc:          func(command string, args []string, workingDir string) error { return nil },
	}
	migratorv4 := NewMigratorV4(w.CliWriter, false, false, mmu, &fileutils.MockFileSystemUtils{}, 10)
	migratorv4.(*MigratorV4).AddDefaultMigrationSteps()
	err := migratorv4.ExecuteMigrationSteps()
	expected := "Can't process without user consent."
	assert.Equal(t, err.Error(), expected)
}

func TestRunMigrationFlowAllSuccess(t *testing.T) {
	w := majorupgrade_utils.NewCustomWriterWithInputs("y", "y")
	mmu := &MockMigratorV4UtilsImpl{
		IsExternalElasticSearchFunc: func(timeout int64) bool { return false },
		StopAutomateFunc:            func() error { return nil },
		GetEsTotalShardSettingsFunc: func() (int32, error) { return 2000, nil },
		PatchOpensearchConfigFunc:   func(es *ESSettings) (string, string, error) { return "", "", nil },
		GetHabRootPathFunc:          func(habrootcmd string) string { return "/hab" },
		ReadV4ChecklistFunc:         func(id string) (bool, error) { return true, nil },
		StartAutomateFunc:           func() error { return nil },
		ExecuteCommandFunc:          func(command string, args []string, workingDir string) error { return nil },
	}
	migratorv4 := NewMigratorV4(w.CliWriter, false, false, mmu, &fileutils.MockFileSystemUtils{
		CalDirSizeInGBFunc:   func(dir string) (float64, error) { return 2.0, nil },
		GetFreeSpaceinGBFunc: func(dir string) (float64, error) { return 3.0, nil },
	}, 10)
	migratorv4.RunMigrationFlow()
	expected1 := "Failed"
	assert.NotContains(t, w.Output(), expected1)
}

func TestRunMigrationFlowIsExecutedErrors(t *testing.T) {
	w := majorupgrade_utils.NewCustomWriterWithInputs("y", "y")
	mmu := &MockMigratorV4UtilsImpl{
		IsExternalElasticSearchFunc: func(timeout int64) bool { return false },
		StopAutomateFunc:            func() error { return nil },
		GetEsTotalShardSettingsFunc: func() (int32, error) { return 2000, nil },
		PatchOpensearchConfigFunc:   func(es *ESSettings) (string, string, error) { return "", "", nil },
		GetHabRootPathFunc:          func(habrootcmd string) string { return "/hab" },
		ReadV4ChecklistFunc:         func(id string) (bool, error) { return true, errors.New("unexpected") },
		StartAutomateFunc:           func() error { return errors.New("unexpected") },
		ExecuteCommandFunc:          func(command string, args []string, workingDir string) error { return nil },
	}
	migratorv4 := NewMigratorV4(w.CliWriter, false, false, mmu, &fileutils.MockFileSystemUtils{
		CalDirSizeInGBFunc:   func(dir string) (float64, error) { return 2.0, nil },
		GetFreeSpaceinGBFunc: func(dir string) (float64, error) { return 3.0, nil },
	}, 10)
	migratorv4.RunMigrationFlow()
	expected1 := `[Error] unexpected
Please resolve this and try again.
Please contact support if you are not sure how to resolve this.
Migration Terminated.`
	assert.Contains(t, w.Output(), expected1)
}

func TestRunMigrationFlowIsExecutedReplyNo(t *testing.T) {
	w := majorupgrade_utils.NewCustomWriterWithInputs("y", "n")
	mmu := &MockMigratorV4UtilsImpl{
		IsExternalElasticSearchFunc: func(timeout int64) bool { return false },
		StopAutomateFunc:            func() error { return nil },
		GetEsTotalShardSettingsFunc: func() (int32, error) { return 2000, nil },
		PatchOpensearchConfigFunc:   func(es *ESSettings) (string, string, error) { return "", "", nil },
		GetHabRootPathFunc:          func(habrootcmd string) string { return "/hab" },
		ReadV4ChecklistFunc:         func(id string) (bool, error) { return true, nil },
		StartAutomateFunc:           func() error { return errors.New("unexpected") },
		ExecuteCommandFunc:          func(command string, args []string, workingDir string) error { return nil },
	}
	migratorv4 := NewMigratorV4(w.CliWriter, false, false, mmu, &fileutils.MockFileSystemUtils{
		CalDirSizeInGBFunc:   func(dir string) (float64, error) { return 2.0, nil },
		GetFreeSpaceinGBFunc: func(dir string) (float64, error) { return 3.0, nil },
	}, 10)
	migratorv4.RunMigrationFlow()
	expected1 := `Migration Terminated.`
	assert.Contains(t, w.Output(), expected1)
}

func TestRunMigrationFlowDefferedErrors(t *testing.T) {
	w := majorupgrade_utils.NewCustomWriterWithInputs("y", "y")
	mmu := &MockMigratorV4UtilsImpl{
		IsExternalElasticSearchFunc: func(timeout int64) bool { return false },
		StopAutomateFunc:            func() error { return nil },
		GetEsTotalShardSettingsFunc: func() (int32, error) { return 2000, nil },
		PatchOpensearchConfigFunc:   func(es *ESSettings) (string, string, error) { return "", "", nil },
		GetHabRootPathFunc:          func(habrootcmd string) string { return "/hab" },
		ReadV4ChecklistFunc:         func(id string) (bool, error) { return true, nil },
		StartAutomateFunc:           func() error { return errors.New("unexpected error while starting automate") },
		ExecuteCommandFunc:          func(command string, args []string, workingDir string) error { return nil },
	}
	migratorv4 := NewMigratorV4(w.CliWriter, false, false, mmu, &fileutils.MockFileSystemUtils{
		CalDirSizeInGBFunc:   func(dir string) (float64, error) { return 2.0, nil },
		GetFreeSpaceinGBFunc: func(dir string) (float64, error) { return 3.0, nil },
	}, 10)
	migratorv4.RunMigrationFlow()
	expected1 := `[Error] unexpected error while starting automate
Please resolve this and try again.
Please contact support if you are not sure how to resolve this.
Migration Terminated.`
	assert.Contains(t, w.Output(), expected1)
}

func TestRunMigrationFlowDefferedErrorsAndExecuteMigrationError(t *testing.T) {
	w := majorupgrade_utils.NewCustomWriterWithInputs("y", "y")
	mmu := &MockMigratorV4UtilsImpl{
		IsExternalElasticSearchFunc: func(timeout int64) bool { return false },
		StopAutomateFunc:            func() error { return nil },
		GetEsTotalShardSettingsFunc: func() (int32, error) { return 2000, nil },
		PatchOpensearchConfigFunc:   func(es *ESSettings) (string, string, error) { return "", "", nil },
		GetHabRootPathFunc:          func(habrootcmd string) string { return "/hab" },
		ReadV4ChecklistFunc:         func(id string) (bool, error) { return true, nil },
		StartAutomateFunc:           func() error { return errors.New("unexpected error while starting automate") },
		ExecuteCommandFunc: func(command string, args []string, workingDir string) error {
			if args[0] == "-c" {
				return errors.New("unexpected error while running scripts")
			}
			return nil
		},
		GetServicesStatusFunc: func() (bool, error) { return true, nil },
	}
	migratorv4 := NewMigratorV4(w.CliWriter, false, false, mmu, &fileutils.MockFileSystemUtils{
		CalDirSizeInGBFunc:   func(dir string) (float64, error) { return 2.0, nil },
		GetFreeSpaceinGBFunc: func(dir string) (float64, error) { return 3.0, nil },
		PathExistsFunc:       func(path string) (bool, error) { return true, nil },
	}, 10)
	migratorv4.RunMigrationFlow()
	expected1 := `✖  Failed to start Chef Automate
[Error] Failed to copy data while migration: unexpected error while running scripts
[Error] unexpected error while starting automate
Please resolve this and try again.
Please contact support if you are not sure how to resolve this.
Migration Terminated.
`
	assert.Contains(t, w.Output(), expected1)
}
