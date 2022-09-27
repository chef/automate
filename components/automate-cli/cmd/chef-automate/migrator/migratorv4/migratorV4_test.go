package migratorV4

import (
	"testing"

	"github.com/chef/automate/lib/majorupgrade_utils"
	"github.com/stretchr/testify/assert"
)

func TestRunMigrations(t *testing.T) {
	w := majorupgrade_utils.NewCustomWriterWithInputs("y")
	mmu := &MockMigratorV4UtilsImpl{}
	migratorv4 := NewMigratorV4(w.CliWriter, false, false, mmu, 10)
	migratorv4.(*MigratorV4).AddDefaultMigrationSteps()
	migratorv4.AskForConfirmation()
	t.Log(w.Output())
	assert.Equal(t, w.Output(), "Do you wish to migrate the Elasticsearch data to OpenSearch now? (y/n)\n")
}

func TestRunMigrationsWronginput(t *testing.T) {
	w := majorupgrade_utils.NewCustomWriterWithInputs("x")
	mmu := &MockMigratorV4UtilsImpl{}
	migratorv4 := NewMigratorV4(w.CliWriter, false, false, mmu, 10)
	migratorv4.(*MigratorV4).AddDefaultMigrationSteps()
	migratorv4.AskForConfirmation()
	t.Log(w.Output())
	assert.Equal(t, w.Output(), `Do you wish to migrate the Elasticsearch data to OpenSearch now? (y/n)
I don't understand 'x'. Please type 'y' or 'n'.
Do you wish to migrate the Elasticsearch data to OpenSearch now? (y/n)
`)
}

func TestRunSuccessfulMigrations(t *testing.T) {
	w := majorupgrade_utils.NewCustomWriterWithInputs("y")
	mmu := &MockMigratorV4UtilsImpl{
		StopAutomateFunc:            func() error { return nil },
		GetEsTotalShardSettingsFunc: func() (int32, error) { return 2000, nil },
		PatchOpensearchConfigFunc:   func(es *ESSettings) (string, string, error) { return "", "", nil },
		GetHabRootPathFunc:          func(habrootcmd string) string { return "/hab" },
		ExecShCommandFunc:           func(script string) error { return nil },
	}
	migratorv4 := NewMigratorV4(w.CliWriter, false, false, mmu, 10)
	migratorv4.(*MigratorV4).AddDefaultMigrationSteps()
	migratorv4.AskForConfirmation()
	migratorv4.ExecuteMigrationSteps()
	expected1 := "Stopping Chef Automate"
	expected2 := "✔  Chef Automate Stopped"
	expected3 := "Updating OpenSearch configurations"
	expected4 := "✔  OpenSearch configurations updated successfully"
	expected5 := "Copying Data"
	expected6 := "✔  Data Copied Successfully"

	assert.Contains(t, w.Output(), "Do you wish to migrate the Elasticsearch data to OpenSearch now? (y/n)\n")
	assert.Contains(t, w.Output(), expected1)
	assert.Contains(t, w.Output(), expected2)
	assert.Contains(t, w.Output(), expected3)
	assert.Contains(t, w.Output(), expected4)
	assert.Contains(t, w.Output(), expected5)
	assert.Contains(t, w.Output(), expected6)

}
