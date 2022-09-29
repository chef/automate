package migratorv4

import (
	"errors"
	"testing"
	"time"

	"github.com/chef/automate/lib/io/fileutils"
	"github.com/chef/automate/lib/majorupgrade_utils"
	"github.com/stretchr/testify/assert"
)

const WISH_TO_MIGRATE = "Do you wish to migrate the Elasticsearch data to OpenSearch now? (y/n)\n"

func TestRunMigrations(t *testing.T) {
	w := majorupgrade_utils.NewCustomWriterWithInputs("y")
	mmu := &MockMigratorV4UtilsImpl{}
	migratorv4 := NewMigratorV4(w.CliWriter, mmu, &fileutils.MockFileSystemUtils{}, 10, 100*time.Millisecond)
	migratorv4.(*MigratorV4).AddDefaultMigrationSteps()
	migratorv4.AskForConfirmation(false)
	t.Log(w.Output())
	assert.Equal(t, w.Output(), WISH_TO_MIGRATE)
}

func TestRunMigrationsWronginput(t *testing.T) {
	w := majorupgrade_utils.NewCustomWriterWithInputs("x")
	mmu := &MockMigratorV4UtilsImpl{}
	migratorv4 := NewMigratorV4(w.CliWriter, mmu, &fileutils.MockFileSystemUtils{}, 10, 100*time.Millisecond)
	migratorv4.(*MigratorV4).AddDefaultMigrationSteps()
	migratorv4.AskForConfirmation(false)
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
		GetEsTotalShardSettingsFunc: func(string) (int32, error) { return 2000, nil },
		PatchOpensearchConfigFunc:   func(es *ESSettings) (string, string, error) { return "", "", nil },
		ExecuteCommandFunc:          func(command string, args []string, workingDir string) error { return nil },
		GetServicesStatusFunc:       func() (bool, error) { return true, nil },
	}
	migratorv4 := NewMigratorV4(w.CliWriter, mmu, &fileutils.MockFileSystemUtils{
		CalDirSizeInGBFunc:   func(path string) (float64, error) { return 3, nil },
		GetFreeSpaceinGBFunc: func(dir string) (float64, error) { return 4, nil },
		PathExistsFunc:       func(path string) (bool, error) { return true, nil },
		GetHabRootPathFunc:   func() string { return majorupgrade_utils.HAB_DIR },
	}, 10, 100*time.Millisecond)
	migratorv4.(*MigratorV4).AddDefaultMigrationSteps()
	migratorv4.AskForConfirmation(false)
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
		GetEsTotalShardSettingsFunc: func(string) (int32, error) { return 2000, nil },
		PatchOpensearchConfigFunc:   func(es *ESSettings) (string, string, error) { return "", "", nil },
		ExecuteCommandFunc:          func(command string, args []string, workingDir string) error { return nil },
	}
	migratorv4 := NewMigratorV4(w.CliWriter, mmu, &fileutils.MockFileSystemUtils{
		GetHabRootPathFunc: func() string { return majorupgrade_utils.HAB_DIR },
	}, 10, 100*time.Millisecond)
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
		GetEsTotalShardSettingsFunc: func(string) (int32, error) { return 2000, nil },
		PatchOpensearchConfigFunc:   func(es *ESSettings) (string, string, error) { return "", "", nil },
		ReadV4ChecklistFunc:         func(id, path string) (bool, error) { return true, nil },
		StartAutomateFunc:           func() error { return nil },
		ExecuteCommandFunc:          func(command string, args []string, workingDir string) error { return nil },
		GetServicesStatusFunc:       func() (bool, error) { return true, nil },
		GetAutomateFQDNFunc:         func(timeout int64) string { return "http://automate.io" },
		UpdatePostChecklistFileFunc: func(id, path string) error { return nil },
	}
	migratorv4 := NewMigratorV4(w.CliWriter, mmu, &fileutils.MockFileSystemUtils{
		CalDirSizeInGBFunc:   func(dir string) (float64, error) { return 2.0, nil },
		GetFreeSpaceinGBFunc: func(dir string) (float64, error) { return 3.0, nil },
		PathExistsFunc:       func(path string) (bool, error) { return true, nil },
		GetHabRootPathFunc:   func() string { return majorupgrade_utils.HAB_DIR },
	}, 10, 100*time.Millisecond)
	migratorv4.RunMigrationFlow(false)
	expected1 := "Failed"
	assert.NotContains(t, w.Output(), expected1)
}

func TestRunMigrationFlowIsExecutedErrors(t *testing.T) {
	w := majorupgrade_utils.NewCustomWriterWithInputs("y", "y")
	mmu := &MockMigratorV4UtilsImpl{
		IsExternalElasticSearchFunc: func(timeout int64) bool { return false },
		StopAutomateFunc:            func() error { return nil },
		GetEsTotalShardSettingsFunc: func(string) (int32, error) { return 2000, nil },
		PatchOpensearchConfigFunc:   func(es *ESSettings) (string, string, error) { return "", "", nil },
		ReadV4ChecklistFunc:         func(id, path string) (bool, error) { return true, errors.New("unexpected") },
		StartAutomateFunc:           func() error { return errors.New("unexpected") },
		ExecuteCommandFunc:          func(command string, args []string, workingDir string) error { return nil },
	}
	migratorv4 := NewMigratorV4(w.CliWriter, mmu, &fileutils.MockFileSystemUtils{
		CalDirSizeInGBFunc:   func(dir string) (float64, error) { return 2.0, nil },
		GetFreeSpaceinGBFunc: func(dir string) (float64, error) { return 3.0, nil },
		GetHabRootPathFunc:   func() string { return majorupgrade_utils.HAB_DIR },
	}, 10, 100*time.Millisecond)
	migratorv4.RunMigrationFlow(false)
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
		GetEsTotalShardSettingsFunc: func(string) (int32, error) { return 2000, nil },
		PatchOpensearchConfigFunc:   func(es *ESSettings) (string, string, error) { return "", "", nil },
		ReadV4ChecklistFunc:         func(id, path string) (bool, error) { return true, nil },
		StartAutomateFunc:           func() error { return errors.New("unexpected") },
		ExecuteCommandFunc:          func(command string, args []string, workingDir string) error { return nil },
	}
	migratorv4 := NewMigratorV4(w.CliWriter, mmu, &fileutils.MockFileSystemUtils{
		CalDirSizeInGBFunc:   func(dir string) (float64, error) { return 2.0, nil },
		GetFreeSpaceinGBFunc: func(dir string) (float64, error) { return 3.0, nil },
		GetHabRootPathFunc:   func() string { return majorupgrade_utils.HAB_DIR },
	}, 10, 100*time.Millisecond)
	migratorv4.RunMigrationFlow(false)
	expected1 := `Migration Terminated.`
	assert.Contains(t, w.Output(), expected1)
}

func TestRunMigrationFlowDefferedErrors(t *testing.T) {
	w := majorupgrade_utils.NewCustomWriterWithInputs("y", "y")
	mmu := &MockMigratorV4UtilsImpl{
		IsExternalElasticSearchFunc: func(timeout int64) bool { return false },
		StopAutomateFunc:            func() error { return nil },
		GetEsTotalShardSettingsFunc: func(string) (int32, error) { return 2000, nil },
		PatchOpensearchConfigFunc:   func(es *ESSettings) (string, string, error) { return "", "", nil },
		ReadV4ChecklistFunc:         func(id, path string) (bool, error) { return true, nil },
		StartAutomateFunc:           func() error { return errors.New("unexpected error while starting automate") },
		ExecuteCommandFunc:          func(command string, args []string, workingDir string) error { return nil },
		GetServicesStatusFunc:       func() (bool, error) { return true, nil },
		UpdatePostChecklistFileFunc: func(id, path string) error { return nil },
	}
	migratorv4 := NewMigratorV4(w.CliWriter, mmu, &fileutils.MockFileSystemUtils{
		CalDirSizeInGBFunc:   func(dir string) (float64, error) { return 2.0, nil },
		GetFreeSpaceinGBFunc: func(dir string) (float64, error) { return 3.0, nil },
		PathExistsFunc:       func(path string) (bool, error) { return true, nil },
		GetHabRootPathFunc:   func() string { return majorupgrade_utils.HAB_DIR },
	}, 10, 100*time.Millisecond)
	migratorv4.RunMigrationFlow(false)
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
		GetEsTotalShardSettingsFunc: func(string) (int32, error) { return 2000, nil },
		PatchOpensearchConfigFunc:   func(es *ESSettings) (string, string, error) { return "", "", nil },
		ReadV4ChecklistFunc:         func(id, path string) (bool, error) { return true, nil },
		StartAutomateFunc:           func() error { return errors.New("unexpected error while starting automate") },
		ExecuteCommandFunc: func(command string, args []string, workingDir string) error {
			if args[0] == "-c" {
				return errors.New("unexpected error while running scripts")
			}
			return nil
		},
		GetServicesStatusFunc:       func() (bool, error) { return true, nil },
		UpdatePostChecklistFileFunc: func(id, path string) error { return nil },
	}
	migratorv4 := NewMigratorV4(w.CliWriter, mmu, &fileutils.MockFileSystemUtils{
		CalDirSizeInGBFunc:   func(dir string) (float64, error) { return 2.0, nil },
		GetFreeSpaceinGBFunc: func(dir string) (float64, error) { return 3.0, nil },
		PathExistsFunc:       func(path string) (bool, error) { return true, nil },
		GetHabRootPathFunc:   func() string { return majorupgrade_utils.HAB_DIR },
	}, 10, 100*time.Millisecond)
	migratorv4.RunMigrationFlow(false)
	expected1 := `✖  Failed to start Chef Automate
[Error] Failed to copy data while migration: unexpected error while running scripts
[Error] unexpected error while starting automate
Please resolve this and try again.
Please contact support if you are not sure how to resolve this.
Migration Terminated.
`
	assert.Contains(t, w.Output(), expected1)
}

func TestRunMigrationFlowCleanupDecline(t *testing.T) {
	w := majorupgrade_utils.NewCustomWriterWithInputs("y", "y", "y", "n")
	mmu := &MockMigratorV4UtilsImpl{
		IsExternalElasticSearchFunc: func(timeout int64) bool { return false },
		StopAutomateFunc:            func() error { return nil },
		GetEsTotalShardSettingsFunc: func(string) (int32, error) { return 2000, nil },
		PatchOpensearchConfigFunc:   func(es *ESSettings) (string, string, error) { return "", "", nil },
		ReadV4ChecklistFunc:         func(id, path string) (bool, error) { return true, nil },
		StartAutomateFunc:           func() error { return nil },
		ExecuteCommandFunc:          func(command string, args []string, workingDir string) error { return nil },
		GetServicesStatusFunc:       func() (bool, error) { return true, nil },
		UpdatePostChecklistFileFunc: func(id, path string) error { return nil },
		GetAutomateFQDNFunc:         func(timeout int64) string { return "http://automate.io" },
	}
	migratorv4 := NewMigratorV4(w.CliWriter, mmu, &fileutils.MockFileSystemUtils{
		CalDirSizeInGBFunc:   func(dir string) (float64, error) { return 2.0, nil },
		GetFreeSpaceinGBFunc: func(dir string) (float64, error) { return 3.0, nil },
		PathExistsFunc:       func(path string) (bool, error) { return true, nil },
		GetHabRootPathFunc:   func() string { return majorupgrade_utils.HAB_DIR },
	}, 10, 100*time.Millisecond)
	migratorv4.RunMigrationFlow(false)
	t.Log(w.Output())
	expected1 := "Failed"
	expected2 := "✔  Clean up successful"
	assert.NotContains(t, w.Output(), expected1)
	assert.NotContains(t, w.Output(), expected2)
}

func TestRunMigrationFlowCleanup(t *testing.T) {
	w := majorupgrade_utils.NewCustomWriterWithInputs("y", "y", "y", "y")
	mmu := &MockMigratorV4UtilsImpl{
		IsExternalElasticSearchFunc: func(timeout int64) bool { return false },
		StopAutomateFunc:            func() error { return nil },
		GetEsTotalShardSettingsFunc: func(string) (int32, error) { return 2000, nil },
		PatchOpensearchConfigFunc:   func(es *ESSettings) (string, string, error) { return "", "", nil },
		ReadV4ChecklistFunc:         func(id, path string) (bool, error) { return true, nil },
		StartAutomateFunc:           func() error { return nil },
		ExecuteCommandFunc:          func(command string, args []string, workingDir string) error { return nil },
		GetServicesStatusFunc:       func() (bool, error) { return true, nil },
		UpdatePostChecklistFileFunc: func(id, path string) error { return nil },
		GetAutomateFQDNFunc:         func(timeout int64) string { return "http://automate.io" },
	}
	migratorv4 := NewMigratorV4(w.CliWriter, mmu, &fileutils.MockFileSystemUtils{
		CalDirSizeInGBFunc:   func(dir string) (float64, error) { return 2.0, nil },
		GetFreeSpaceinGBFunc: func(dir string) (float64, error) { return 3.0, nil },
		PathExistsFunc:       func(path string) (bool, error) { return true, nil },
		GetHabRootPathFunc:   func() string { return majorupgrade_utils.HAB_DIR },
	}, 10, 100*time.Millisecond)
	migratorv4.RunMigrationFlow(false)
	t.Log(w.Output())
	expected1 := "Failed"
	expected2 := "✔  Clean up successful"
	assert.NotContains(t, w.Output(), expected1)
	assert.Contains(t, w.Output(), expected2)
}

func TestPathNotExist(t *testing.T) {
	w := majorupgrade_utils.NewCustomWriterWithInputs("y", "y")
	mmu := &MockMigratorV4UtilsImpl{
		StopAutomateFunc:            func() error { return nil },
		GetEsTotalShardSettingsFunc: func(string) (int32, error) { return 2000, nil },
		PatchOpensearchConfigFunc:   func(es *ESSettings) (string, string, error) { return "", "", nil },
		ExecuteCommandFunc:          func(command string, args []string, workingDir string) error { return nil },
		GetServicesStatusFunc:       func() (bool, error) { return true, nil },
		IsExternalElasticSearchFunc: func(timeout int64) bool { return false },
		ReadV4ChecklistFunc:         func(id, path string) (bool, error) { return true, nil },
		UpdatePostChecklistFileFunc: func(id, path string) error { return nil },
	}
	migratorv4 := NewMigratorV4(w.CliWriter, mmu, &fileutils.MockFileSystemUtils{
		CalDirSizeInGBFunc:   func(path string) (float64, error) { return 3, nil },
		GetFreeSpaceinGBFunc: func(dir string) (float64, error) { return 4, nil },
		PathExistsFunc:       func(path string) (bool, error) { return true, errors.New("path error") },
		GetHabRootPathFunc:   func() string { return majorupgrade_utils.HAB_DIR },
	}, 10, 100*time.Millisecond)
	migratorv4.RunMigrationFlow(false)
	expected1 := "[Error] Failed to check directory /hab/svc/automate-elasticsearch/data: path error\nMigration Terminated.\n"

	assert.Contains(t, w.Output(), WISH_TO_MIGRATE)
	assert.Contains(t, w.Output(), expected1)

}

func TestExternalDetected(t *testing.T) {
	w := majorupgrade_utils.NewCustomWriterWithInputs("y", "y")
	mmu := &MockMigratorV4UtilsImpl{
		StopAutomateFunc:            func() error { return nil },
		GetEsTotalShardSettingsFunc: func(string) (int32, error) { return 2000, nil },
		PatchOpensearchConfigFunc:   func(es *ESSettings) (string, string, error) { return "", "", nil },
		ExecuteCommandFunc:          func(command string, args []string, workingDir string) error { return nil },
		GetServicesStatusFunc:       func() (bool, error) { return true, nil },
		IsExternalElasticSearchFunc: func(timeout int64) bool { return true },
		ReadV4ChecklistFunc:         func(id, path string) (bool, error) { return true, nil },
	}
	migratorv4 := NewMigratorV4(w.CliWriter, mmu, &fileutils.MockFileSystemUtils{
		CalDirSizeInGBFunc:   func(path string) (float64, error) { return 3, nil },
		GetFreeSpaceinGBFunc: func(dir string) (float64, error) { return 4, nil },
		PathExistsFunc:       func(path string) (bool, error) { return true, errors.New("path error") },
		GetHabRootPathFunc:   func() string { return majorupgrade_utils.HAB_DIR },
	}, 10, 100*time.Millisecond)
	migratorv4.RunMigrationFlow(false)
	expected := "[Error] Detected External OpenSearch\nPlease resolve this and try again.\nPlease contact support if you are not sure how to resolve this.\nMigration Terminated.\n"
	assert.Contains(t, w.Output(), expected)

}
