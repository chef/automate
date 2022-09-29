package migratorV4

import (
	"errors"

	"github.com/chef/automate/components/automate-cli/cmd/chef-automate/migrator"
	"github.com/chef/automate/components/automate-deployment/pkg/cli"
	"github.com/chef/automate/lib/io/fileutils"
)

const (
	NEXT_AUTOMATE_VERSION = "4"
	MIGRATION_TERMINATED  = "Migration Terminated."
)

type MigratorV4 struct {
	*MigrateV4Flags
	timeout          int64
	migratorUtils    MigratorV4Utils
	fileutils        fileutils.FileUtils
	writer           *cli.Writer
	migrationSteps   []migrator.MigrationSteps
	migrationConsent bool
	isExecuted       bool
}

type MigrateV4Flags struct {
	AutoAcceptFlag bool
	ForceExecute   bool
}

func NewMigratorV4(writer *cli.Writer, autoAccept, forceExecute bool, migratorUtils MigratorV4Utils, fileutils fileutils.FileUtils, timeout int64) migrator.Migrator {
	return &MigratorV4{
		writer: writer,
		MigrateV4Flags: &MigrateV4Flags{
			AutoAcceptFlag: autoAccept,
			ForceExecute:   forceExecute,
		},
		migratorUtils: migratorUtils,
		fileutils:     fileutils,
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
			step.Skip()
		} else {
			err = step.Run()
		}
	}
	return err
}

func (m *MigratorV4) AddMigrationSteps(migrationSteps migrator.MigrationSteps) {
	if migrationSteps != nil {
		m.migrationSteps = append(m.migrationSteps, migrationSteps)
	}
}

func (m *MigratorV4) AskForConfirmation() error {
	res, err := m.writer.Confirm("Do you wish to migrate the Elasticsearch data to OpenSearch now?")
	if err != nil {
		return err
	}
	if !res {
		return errors.New(MIGRATION_TERMINATED)
	}
	m.migrationConsent = res
	return nil
}

func (m *MigratorV4) AddDefaultMigrationSteps() {
	m.AddMigrationSteps(NewCheckStorage(m.writer, m.migratorUtils, m.fileutils))
	m.AddMigrationSteps(NewPatchOpensearchConfig(m.writer, m.migratorUtils))
	m.AddMigrationSteps(NewAutomateStop(m.writer, m.migratorUtils))
	m.AddMigrationSteps(NewMigrationScript(m.writer, m.migratorUtils))
	m.AddMigrationSteps(NewWaitForHealthy(m.writer, m.migratorUtils))
}

func (m *MigratorV4) ExecuteDeferredSteps() error {
	for _, steps := range m.migrationSteps {
		var i interface{} = steps
		_, ok := i.(migrator.PostMigrationSteps)
		if ok {
			err := steps.(migrator.PostMigrationSteps).DefferedHandler()
			if err != nil {
				return err
			}
		}
	}
	return nil
}

func (m *MigratorV4) PrintMigrationErrors() {
	for _, steps := range m.migrationSteps {
		steps.ErrorHandler()
	}
}

func (m *MigratorV4) SaveExecutedStatus() error {
	return m.migratorUtils.UpdatePostChecklistFile()
}

func (m *MigratorV4) IsExecutedCheck() error {
	isExecuted, err := m.migratorUtils.ReadV4Checklist()
	if err != nil {
		return err
	}
	if isExecuted {
		m.migrationConsent = false
		m.writer.Println("You have already migrated your data from Elasticsearch to OpenSearch.")
		m.writer.Println("Proceeding with migration will delete the existing data in OpenSearch.")
		res, err := m.writer.Confirm("Do you want to Migrate again?")
		if err != nil {
			return err
		}
		if !res {
			return errors.New("Migration Terminated.")
		}
		m.migrationConsent = true
	}
	return nil
}

func (m *MigratorV4) RunMigrationFlow() error {
	err := m.AskForConfirmation()
	if err != nil {
		return err
	}
	err = m.IsExecutedCheck()
	if err != nil {
		return err
	}
	m.AddDefaultMigrationSteps()
	m.ExecuteMigrationSteps()
	err = m.ExecuteDeferredSteps()
	if err != nil {
		m.writer.Println(err.Error())
	}
	m.PrintMigrationErrors()
	return nil
}
