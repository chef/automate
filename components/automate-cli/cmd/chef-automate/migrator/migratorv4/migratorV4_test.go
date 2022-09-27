package migratorV4

import (
	"testing"

	"github.com/bmizerany/assert"
	"github.com/chef/automate/components/automate-deployment/pkg/inspector/upgradeinspectorv4"
)

func TestRunMigrations(t *testing.T) {
	w := upgradeinspectorv4.NewTestWriterWithInputs("y")
	mmu := &MockMigratorV4UtilsImpl{}
	migratorv4 := NewMigratorV4(w.CliWriter, false, false, mmu, 10)
	migratorv4.(*MigratorV4).AddDefaultMigrationSteps()
	migratorv4.AskForConfirmation()
	t.Log(w.Output())
	assert.Equal(t, w.Output(), "Do you wish to migrate the Elasticsearch data to OpenSearch now? (y/n)\n")
}
