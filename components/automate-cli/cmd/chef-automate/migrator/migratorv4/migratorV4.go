package migratorv4

import (
	"errors"
	"time"

	"github.com/chef/automate/components/automate-cli/cmd/chef-automate/migrator"
	"github.com/chef/automate/components/automate-deployment/pkg/cli"
	"github.com/chef/automate/lib/io/fileutils"
	"github.com/chef/automate/lib/majorupgrade_utils"
	"github.com/fatih/color"
)

const (
	MIGRATION_TERMINATED = "Migration Terminated."
)

var SPINNER_TIMEOUT = 100 * time.Millisecond

type MigratorV4 struct {
	timeout          int64
	migratorUtils    MigratorV4Utils
	fileutils        fileutils.FileUtils
	writer           *cli.Writer
	migrationSteps   []migrator.MigrationSteps
	migrationConsent bool
	isExecuted       bool
	runHealthStatus  bool
	spinnerTimeout   time.Duration
}

func NewMigratorV4(writer *cli.Writer, migratorUtils MigratorV4Utils, fileutils fileutils.FileUtils, timeout int64, spinnerTimeout time.Duration) migrator.Migrator {
	return &MigratorV4{
		writer:         writer,
		migratorUtils:  migratorUtils,
		fileutils:      fileutils,
		timeout:        timeout,
		spinnerTimeout: spinnerTimeout,
	}
}

func (m *MigratorV4) ExecuteMigrationSteps() (err error) {
	if !m.migrationConsent {
		return errors.New("Can't process without user consent.")
	}
	m.writer.Println("Data Migration is in progress")
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
	if skipConfirmation {
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
	m.AddMigrationSteps(NewEnableMaintenance(m.writer, m.migratorUtils, m.timeout, m.spinnerTimeout))
	m.AddMigrationSteps(NewPatchOpensearchConfig(m.writer, m.migratorUtils, m.fileutils, m.spinnerTimeout))
	m.AddMigrationSteps(NewCheckDirExists(m.writer, m.fileutils))
	m.AddMigrationSteps(NewAutomateStop(m.writer, m.migratorUtils, &m.runHealthStatus, m.spinnerTimeout))
	m.AddMigrationSteps(NewMigrationScript(m.writer, m.migratorUtils, m.fileutils, m.spinnerTimeout))
	m.AddMigrationSteps(NewWaitForHealthy(m.writer, m.migratorUtils, &m.runHealthStatus, m.spinnerTimeout))
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
	habroot := m.fileutils.GetHabRootPath()
	return m.migratorUtils.UpdatePostChecklistFile(MIGRATE_ES_ID, habroot+majorupgrade_utils.UPGRADE_METADATA)
}

func (m *MigratorV4) IsExecutedCheck() error {
	habroot := m.fileutils.GetHabRootPath()
	isExecuted, err := m.migratorUtils.ReadV4Checklist(MIGRATE_ES_ID, habroot+majorupgrade_utils.UPGRADE_METADATA)
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
	} else {
		m.RunSuccess()
		m.SaveExecutedStatus()
	}
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
		err := m.doDelete()
		if err != nil {
			m.writer.Println(err.Error())
			m.writer.Println("Failed to clean old Elasticsearch data")
		}
	}
}

func (m *MigratorV4) doDelete() error {
	doDelete, err := m.writer.Confirm("Would you like to clean up the old Elasticsearch data now?")
	if err != nil {
		return err
	}
	if !doDelete {
		m.writer.Println("Cleanup skipped")
		m.writer.Println("")
		m.writer.Println("To clean up Elasticsearch data later, use command:")
		m.writer.Println(color.New(color.Bold).Sprint("$ chef-automate post-major-upgrade clear-data --data=es"))
	}
	err = m.ClearData()
	if err != nil {
		return err
	}
	return nil
}

func (m *MigratorV4) ClearData() error {
	clearData := NewCleanUp(m.writer, m.migratorUtils, m.fileutils, false, false, m.spinnerTimeout)
	clearData.Clean(true)
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

func (m *MigratorV4) RunSuccess() error {
	for _, steps := range m.migrationSteps {
		var i interface{} = steps
		_, ok := i.(migrator.SuccessfulMigrationSteps)
		if ok {
			err := steps.(migrator.SuccessfulMigrationSteps).OnSuccess()
			if err != nil {
				return err
			}
		}
	}
	return nil
}

func (m *MigratorV4) SkipMigrationPermanently() error {
	habroot := m.fileutils.GetHabRootPath()
	return m.migratorUtils.UpdatePostChecklistFile(SKIP_MIGRATION_PERMANENTLY_ID, habroot+majorupgrade_utils.UPGRADE_METADATA)
}

func (m *MigratorV4) IsMigrationPermanentlySkipped() (bool, error) {
	habRoot := m.fileutils.GetHabRootPath()
	isExecuted, err := m.migratorUtils.ReadV4Checklist(SKIP_MIGRATION_PERMANENTLY_ID, habRoot+majorupgrade_utils.UPGRADE_METADATA)
	if err != nil {
		return true, nil
	}
	return isExecuted, nil
}
