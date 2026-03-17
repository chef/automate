package upgradeinspectorv5

import (
	"fmt"
	"strings"

	"github.com/fatih/color"
	"github.com/pkg/errors"

	"github.com/chef/automate/components/automate-deployment/pkg/cli"
	"github.com/chef/automate/components/automate-deployment/pkg/inspector"
	"github.com/chef/automate/lib/io/fileutils"
)

type UpgradeInspectorV5 struct {
	writer           *cli.Writer
	inspections      []inspector.Inspection
	osDestDir        string
	skipStorageCheck bool
	upgradeUtils     UpgradeV5Utils
	fileUtils        fileutils.FileUtils
	timeout          int64
	isExternalOS     bool
	isExternalPG     bool
}

const (
	HAB_DIR                              string = "/hab"
	UPGRADE_TERMINATED                   string = "Upgrade process terminated."
	run_chef_automate_upgrade_status_cmd        = `chef-automate upgrade status`
	run_chef_automate_upgrade_status            = `Check the status of your upgrade using:  
     $ ` + run_chef_automate_upgrade_status_cmd + `
   This should return: Automate is up-to-date`
	ui_check       = `Check Automate UI everything is running and all data is visible`
	patch_new_conf = `If your PostgreSQL Connection URL and Credential are changed then update them by putting them in config.toml and patching it in using:
     $ chef-automate config patch config.toml`
	run_chef_automate_status = `Check all services are running using: 
     $ chef-automate status`
	run_pg_data_migrate_cmd = `chef-automate post-major-upgrade migrate --data=pg`
	run_pg_data_migrate     = `Migrate Data from PG 13.5 to PG 17.0 using this command:
     $ ` + run_pg_data_migrate_cmd
	run_pg_data_cleanup = `If you are sure all data is available in Upgraded Automate, then we can free up old PostgreSQL 13.5 Data by running: 
     $ ` + run_pg_data_cleanup_cmd
	run_pg_data_cleanup_cmd = `chef-automate post-major-upgrade clear-data --data=PG`
	POST_UPGRADE_HEADER     = `Post Upgrade Steps:
===================
`
	initMsg = `This is a Major upgrade. 
========================

  1) Embedded OpenSearch v1.x will be upgraded to OpenSearch v2.x
  2) Embedded PostgreSQL v13.5 will be upgraded to version v17.0

===== Your installation is using %s PostgreSQL and %s Opensearch =====
`
)

func (ui *UpgradeInspectorV5) ShowInfo() error {
	dbType := "Embedded"
	osType := "Embedded"
	if ui.isExternalPG {
		dbType = "External"
	}
	if ui.isExternalOS {
		osType = "External"
	}
	ui.writer.Println(fmt.Sprintf(initMsg, dbType, osType))
	if len(ui.inspections) > 0 {
		ui.writer.Println("Before proceeding, please ensure:")
	}
	index := 1
	for _, inspection := range ui.inspections {
		err := inspection.ShowInfo(&index)
		if err != nil {
			return err
		}
	}
	if len(ui.inspections) > 0 {
		ui.writer.Println("")
	}

	if !ui.upgradeUtils.IsExternalOpenSearch(ui.timeout) && (len(ui.osDestDir) == 0 || ui.osDestDir == HAB_DIR) {
		ui.showOSDestDirFlagMsg()
	}
	ui.writer.Println("For more information, visit")
	ui.writer.Println(color.New(color.FgBlue).Sprint("https://docs.chef.io/automate/major_upgrade_5.x/\n"))
	return ui.promptToContinue()
}

func (ui *UpgradeInspectorV5) promptToContinue() error {
	isContinue, err := ui.writer.Confirm("Would you like to proceed with the upgrade?")
	if err != nil {
		return err
	}
	if !isContinue {
		return errors.New(UPGRADE_TERMINATED)
	}
	return nil
}

func (ui *UpgradeInspectorV5) showOSDestDirFlagMsg() {
	ui.writer.Println(`You can always change the OpenSearch destination directory by using the flag:
  $ chef-automate upgrade run --major --os-dest-data-dir <path to new directory>
`)
}

func (ui *UpgradeInspectorV5) checkInstallationTypeForExternal(installationType inspector.InstallationType) bool {
	return ui.isExternalOS && (installationType == inspector.EXTERNAL || installationType == inspector.BOTH)
}

func (ui *UpgradeInspectorV5) checkInstallationTypeForEmbedded(installationType inspector.InstallationType) bool {
	return !ui.isExternalOS && (installationType == inspector.EMBEDDED || installationType == inspector.BOTH)
}

func (ui *UpgradeInspectorV5) Inspect() (err error) {
	ui.writer.Println("Pre flight checks")
	for _, inspection := range ui.inspections {
		var i interface{} = inspection
		_, ok := i.(inspector.SystemInspection)
		if ok {
			installationType := inspection.(inspector.SystemInspection).GetInstallationType()
			if ui.checkInstallationTypeForExternal(installationType) || ui.checkInstallationTypeForEmbedded(installationType) {
				if err != nil {
					inspection.(inspector.SystemInspection).Skip()
				} else {
					err = inspection.(inspector.SystemInspection).Inspect()
				}
			}
		}
	}
	if err != nil {
		return err
	}
	return nil
}

func (ui *UpgradeInspectorV5) RollBackChangesOnError() (err error) {
	for _, inspection := range ui.inspections {
		var i interface{} = inspection
		_, ok := i.(inspector.RollbackInspection)
		if ok {
			err = inspection.(inspector.RollbackInspection).RollBackHandler()
			if err != nil {
				return err
			}
		}
	}
	return nil
}

func NewUpgradeInspectorV5(w *cli.Writer, upgradeUtils UpgradeV5Utils, fileUtils fileutils.FileUtils, timeout int64) inspector.Inspector {

	return &UpgradeInspectorV5{
		writer:       w,
		upgradeUtils: upgradeUtils,
		fileUtils:    fileUtils,
		timeout:      timeout,
		isExternalOS: upgradeUtils.IsExternalOpenSearch(timeout),
		isExternalPG: upgradeUtils.IsExternalPG(),
	}
}

func (ui *UpgradeInspectorV5) SetOSDestDir(path string) {
	if strings.TrimSpace(path) != "" {
		ui.osDestDir = path
	}
}

func (ui *UpgradeInspectorV5) SetSkipStoragecheckFlag(check bool) {
	ui.skipStorageCheck = check
}

func (ui *UpgradeInspectorV5) AddInspection(inspection inspector.Inspection) {
	if inspection != nil {
		ui.inspections = append(ui.inspections, inspection)
	}
}

func (ui *UpgradeInspectorV5) AddDefaultInspections() {
	ui.AddInspection(NewEnsureStatusInspection(ui.writer, ui.upgradeUtils))
	ui.AddInspection(NewPlannedDownTimeInspection(ui.writer))
	ui.AddInspection(NewTakeBackupInspection(ui.writer))
	if !ui.skipStorageCheck {
		diskSpaceInspection := NewDiskSpaceInspection(ui.writer, ui.isExternalOS, ui.isExternalPG, ui.osDestDir, ui.fileUtils)
		ui.AddInspection(diskSpaceInspection)
	}
	ui.AddInspection(NewExternalPGUpgradeCheckInspection(ui.writer, ui.isExternalPG))
	ui.AddInspection(NewReindexingInspection(ui.writer, ui.isExternalOS))
	ui.AddInspection(NewPostChecklistIntimationCheckInspection(ui.writer))
	ui.AddInspection(NewDisableShardingInspection(ui.writer, ui.upgradeUtils))
	ui.AddInspection(NewEnableMaintenanceInspection(ui.writer, ui.upgradeUtils, ui.timeout))
}

func (ui *UpgradeInspectorV5) ShowInspectionList() {
	ui.writer.Println("\nFollowing Pre-flight checks will be conducted")
	index := 1
	for _, inspection := range ui.inspections {
		var i interface{} = inspection
		_, ok := i.(inspector.SystemInspection)
		if ok {
			installationType := inspection.(inspector.SystemInspection).GetInstallationType()
			if ui.checkInstallationTypeForExternal(installationType) || ui.checkInstallationTypeForEmbedded(installationType) {
				msgs := inspection.(inspector.SystemInspection).GetShortInfo()
				for _, msg := range msgs {
					if msg != "" {
						ui.writer.Printf("%d. %s\n", index, msg)
						index++
					}
				}
			}
		}
	}
	ui.writer.Println("")
}

func (ui *UpgradeInspectorV5) RunExitAction() error {
	for _, inspection := range ui.inspections {
		var i interface{} = inspection
		_, ok := i.(inspector.SystemInspection)
		if ok {
			err := inspection.(inspector.SystemInspection).ExitHandler()
			if err != nil {
				return err
			}
		}
	}
	return nil
}

func (ui *UpgradeInspectorV5) handleError(errArray []error) {
	if len(errArray) == 1 {
		if errArray[0].Error() != UPGRADE_TERMINATED {
			ui.writer.Println("[" + color.New(color.FgRed).Sprint("Error") + "] " + errArray[0].Error())
			ui.printContactSupport()
		}
	} else if len(errArray) > 1 {
		ui.writer.Println("[" + color.New(color.FgRed).Sprint("Error") + "] " + errArray[len(errArray)-1].Error())
		for i := len(errArray) - 1; i >= 0; i-- {
			if i == len(errArray)-1 {
				ui.writer.Println("[" + color.New(color.FgRed).Sprint("Error") + "] " + errArray[i].Error())
			} else {
				ui.writer.Println(errArray[i].Error())
			}
		}
		ui.printContactSupport()
	}

}

func (ui *UpgradeInspectorV5) printContactSupport() {
	ui.writer.Println("Please resolve this and try again.")
	ui.writer.Println("Please contact support if you are not sure how to resolve this.")
}

func (ui *UpgradeInspectorV5) postUpgradeCheckList() {
	checkList := []string{
		run_chef_automate_upgrade_status,
		ui_check,
	}

	if ui.isExternalPG {
		checkList = append(checkList, patch_new_conf, run_chef_automate_status)
	} else {
		checkList = append(checkList, run_pg_data_migrate, run_pg_data_cleanup)
	}

	ui.writer.Println(POST_UPGRADE_HEADER)
	for i, check := range checkList {
		ui.writer.Println(fmt.Sprintf("%d", i+1) + ") " + check + "\n")
	}
}

func (ui *UpgradeInspectorV5) RunUpgradeInspector(osDestDir string, skipStorageCheck bool) (isError bool) {
	errArray := []error{}
	ui.SetOSDestDir(osDestDir)
	ui.SetSkipStoragecheckFlag(skipStorageCheck)
	ui.AddDefaultInspections()
	err := ui.ShowInfo()
	if err != nil {
		ui.handleError(append(errArray, err))
		ui.writer.Println(UPGRADE_TERMINATED)
		return true
	}
	ui.ShowInspectionList()
	err = ui.Inspect()
	if err != nil {
		ui.writer.Println("")
		err = ui.RollBackChangesOnError()
		if err != nil {
			errArray = append(errArray, err)
		}
		err = ui.RunExitAction()
		if err != nil {
			errArray = append(errArray, err)
		}
		if len(errArray) > 0 {
			ui.handleError(errArray)
		}
		ui.writer.Println(UPGRADE_TERMINATED)
		return true
	}
	ui.postUpgradeCheckList()
	return false
}
