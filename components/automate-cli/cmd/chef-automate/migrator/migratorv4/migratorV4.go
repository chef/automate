package migratorv4

import (
	"errors"

	"github.com/chef/automate/components/automate-cli/cmd/chef-automate/migrator"
	"github.com/chef/automate/components/automate-deployment/pkg/cli"
	"github.com/chef/automate/lib/io/fileutils"
	"github.com/fatih/color"
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
	runHealthStatus  bool
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
		err = step.Run()
		if err != nil {
			return err
		}
	}
	return nil
}

func (m *MigratorV4) AddMigrationSteps(migrationSteps migrator.MigrationSteps) {
	if migrationSteps != nil {
		m.migrationSteps = append(m.migrationSteps, migrationSteps)
	}
}

func (m *MigratorV4) AskForConfirmation(skipConfirmation bool) error {
	if skipConfirmation || m.AutoAcceptFlag || m.ForceExecute {
		m.migrationConsent = true
		return nil
	}
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
	m.AddMigrationSteps(NewEnsureStatus(m.writer, m.migratorUtils))
	m.AddMigrationSteps(NewCheckStorage(m.writer, m.migratorUtils, m.fileutils))
	m.AddMigrationSteps(NewPatchOpensearchConfig(m.writer, m.migratorUtils))
	m.AddMigrationSteps(NewCheckDirExists(m.writer, m.fileutils))
	m.AddMigrationSteps(NewAutomateStop(m.writer, m.migratorUtils, &m.runHealthStatus))
	m.AddMigrationSteps(NewMigrationScript(m.writer, m.migratorUtils))
	m.AddMigrationSteps(NewWaitForHealthy(m.writer, m.migratorUtils, &m.runHealthStatus))
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
	return m.migratorUtils.UpdatePostChecklistFile(MIGRATE_ES_ID)
}

func (m *MigratorV4) IsExecutedCheck() error {
	if m.ForceExecute {
		return nil
	}
	isExecuted, err := m.migratorUtils.ReadV4Checklist(MIGRATE_ES_ID)
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
			return errors.New(MIGRATION_TERMINATED)
		}
		m.migrationConsent = true
	}
	return nil
}

func (m *MigratorV4) handleError(err error) {
	if err.Error() == MIGRATION_TERMINATED {
		m.writer.Println(MIGRATION_TERMINATED)
	} else {
		m.writer.Println("[" + color.New(color.FgRed).Sprint("Error") + "] " + err.Error())
		m.writer.Println("Please resolve this and try again.")
		m.writer.Println("Please contact support if you are not sure how to resolve this.")
		m.writer.Println(MIGRATION_TERMINATED)
	}
}

func (m *MigratorV4) RunMigrationFlow(skipConfirmation bool) {
	isExternal := m.migratorUtils.IsExternalElasticSearch(m.timeout)
	if isExternal {
		m.handleError(errors.New("Detected External OpenSearch"))
		return
	}
	err := m.AskForConfirmation(skipConfirmation)
	if err != nil {
		m.handleError(err)
		return
	}
	err = m.IsExecutedCheck()
	if err != nil {
		m.handleError(err)
		return
	}
	m.AddDefaultMigrationSteps()
	err = m.ExecuteMigrationSteps()
	errDeffered := m.ExecuteDeferredSteps()
	if err != nil {
		m.PrintMigrationErrors()
	}
	m.SaveExecutedStatus()
	if errDeffered != nil {
		m.handleError(errDeffered)
	} else {
		if err != nil {
			m.writer.Println(MIGRATION_TERMINATED)
		}
	}
	if err == nil && errDeffered == nil {
		m.writer.Println(" " + color.New(color.FgGreen).Sprint("✔") + "  Migration complete")
		m.showVerifyAutomate()
		err := m.ClearData()
		if err != nil {
			m.writer.Println(err.Error())
		}
	}
}

func (m *MigratorV4) ClearData() error {
	clearData := NewCleanUp(m.writer, m.migratorUtils, false, false)
	clearData.Clean()
	return nil
}

func (m *MigratorV4) showVerifyAutomate() {
	fqdn := m.migratorUtils.GetAutomateFQDN(m.timeout)
	m.writer.Printf(`
Verify Chef Automate to see that everything is running and that all your data is available.
%s

Once verified, you can remove old Elasticsearch data.

`, fqdn)
}
