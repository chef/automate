package migratorV4

import (
	"errors"

	"github.com/chef/automate/components/automate-cli/cmd/chef-automate/migrator"
	"github.com/chef/automate/components/automate-deployment/pkg/cli"
)

type MigratorV4 struct {
	*MigrateV4Flags
	timeout          int64
	migratorUtils    MigratorV4Utils
	writer           *cli.Writer
	migrationSteps   []migrator.MigrationSteps
	migrationConsent bool
}

type MigrateV4Flags struct {
	AutoAcceptFlag bool
	ForceExecute   bool
}

func NewMigratorV4(writer *cli.Writer, autoAccept, forceExecute bool, migratorUtils MigratorV4Utils, timeout int64) migrator.Migrator {
	return &MigratorV4{
		writer: writer,
		MigrateV4Flags: &MigrateV4Flags{
			AutoAcceptFlag: autoAccept,
			ForceExecute:   forceExecute,
		},
		migratorUtils: migratorUtils,
		timeout:       timeout,
	}
}

func (m *MigratorV4) ExecuteMigrationSteps() (err error) {
	if !m.migrationConsent {
		return errors.New("Can't process without user consent.")
	}
	m.writer.Println("Migration in progress")
	for _, step := range m.migrationSteps {
		if err != nil {
			step.(migrator.MigrationSteps).Skip()
		} else {
			err = step.(migrator.MigrationSteps).Run()
		}
	}
	return err
}

func (m *MigratorV4) AddMigrationSteps(migrationSteps migrator.MigrationSteps) {
	if migrationSteps != nil {
		m.migrationSteps = append(m.migrationSteps, migrationSteps)
	}
}

func (m *MigratorV4) AskForConfirmation() (bool, error) {
	res, err := m.writer.Confirm("Do you wish to migrate the Elasticsearch data to OpenSearch now?")
	if err != nil {
		return false, err
	}
	m.migrationConsent = res
	return res, nil
}

func (m *MigratorV4) AddDefaultMigrationSteps() {
	m.AddMigrationSteps(NewAutomateStop(m.writer, m.migratorUtils))
	m.AddMigrationSteps(NewPatchOpensearchConfig(m.writer, m.migratorUtils))
	m.AddMigrationSteps(NewMigrationScript(m.writer, m.migratorUtils))
}

func (m *MigratorV4) ExecuteDeferredSteps() error {
	return nil
}

func (m *MigratorV4) PrintMigrationErrors() error {
	return nil
}
