package upgradeinspectorv4

import (
	"strings"

	"github.com/fatih/color"
	"github.com/pkg/errors"

	"github.com/chef/automate/components/automate-deployment/pkg/cli"
	"github.com/chef/automate/components/automate-deployment/pkg/inspector"
	"github.com/chef/automate/lib/io/fileutils"
)

type UpgradeInspectorV4 struct {
	writer           *cli.Writer
	inspections      []inspector.Inspection
	osDestDir        string
	skipStorageCheck bool
	upgradeUtils     UpgradeV4Utils
	fileUtils        fileutils.FileUtils
	timeout          int64
	isExternal       bool
}

const (
	HAB_DIR            string = "/hab"
	UPGRADE_TERMINATED string = "Upgrade process terminated."
)

func (ui *UpgradeInspectorV4) ShowInfo() error {
	ui.writer.Println(`This is a major upgrade!
In this release, Elasticsearch will be migrated to OpenSearch.
`)
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

	if !ui.upgradeUtils.IsExternalElasticSearch(ui.timeout) && (len(ui.osDestDir) == 0 || ui.osDestDir == HAB_DIR) {
		ui.showOSDestDirFlagMsg()
	}
	ui.writer.Println("For more information, visit")
	ui.writer.Println(color.New(color.FgBlue).Sprint("https://docs.chef.io/automate/major_upgrade_4.x/\n"))
	return ui.promptToContinue()
}

func (ui *UpgradeInspectorV4) promptToContinue() error {
	isContinue, err := ui.writer.Confirm("Would you like to proceed with the upgrade?")
	if err != nil {
		return err
	}
	if !isContinue {
		return errors.New(UPGRADE_TERMINATED)
	}
	return nil
}

func (ui *UpgradeInspectorV4) showOSDestDirFlagMsg() {
	ui.writer.Println(`You can always change the OpenSearch destination directory by using the flag:
  $ chef-automate upgrade run --major --os-dest-data-dir <path to new directory>
`)
}

func (ui *UpgradeInspectorV4) checkInstallationTypeForExternal(installationType inspector.InstallationType) bool {
	return ui.isExternal && (installationType == inspector.EXTERNAL || installationType == inspector.BOTH)
}

func (ui *UpgradeInspectorV4) checkInstallationTypeForEmbedded(installationType inspector.InstallationType) bool {
	return !ui.isExternal && (installationType == inspector.EMBEDDED || installationType == inspector.BOTH)
}

func (ui *UpgradeInspectorV4) Inspect() (err error) {
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

func (ui *UpgradeInspectorV4) RollBackChangesOnError() (err error) {
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

func NewUpgradeInspectorV4(w *cli.Writer, upgradeUtils UpgradeV4Utils, fileUtils fileutils.FileUtils, timeout int64) inspector.Inspector {

	return &UpgradeInspectorV4{
		writer:       w,
		upgradeUtils: upgradeUtils,
		fileUtils:    fileUtils,
		timeout:      timeout,
		isExternal:   upgradeUtils.IsExternalElasticSearch(timeout),
	}
}

func (ui *UpgradeInspectorV4) SetOSDestDir(path string) {
	if strings.TrimSpace(path) != "" {
		ui.osDestDir = path
	}
}

func (ui *UpgradeInspectorV4) SetSkipStoragecheckFlag(check bool) {
	ui.skipStorageCheck = check
}

func (ui *UpgradeInspectorV4) AddInspection(inspection inspector.Inspection) {
	if inspection != nil {
		ui.inspections = append(ui.inspections, inspection)
	}
}

func (ui *UpgradeInspectorV4) AddDefaultInspections() {
	ui.AddInspection(NewEnsureStatusInspection(ui.writer, ui.upgradeUtils))
	ui.AddInspection(NewPlannedDownTimeInspection(ui.writer))
	ui.AddInspection(NewTakeBackupInspection(ui.writer))
	if !ui.skipStorageCheck {
		diskSpaceInspection := NewDiskSpaceInspection(ui.writer, ui.upgradeUtils.IsExternalElasticSearch(ui.timeout), ui.osDestDir, ui.fileUtils)
		ui.AddInspection(diskSpaceInspection)
	}
	esBasePath := ui.upgradeUtils.GetESBasePath(ui.timeout)
	ui.AddInspection(NewESIndexInspection(ui.writer, ui.upgradeUtils, esBasePath))
	ui.AddInspection(NewReplaceS3UrlInspection(ui.writer, ui.upgradeUtils, ui.timeout))
	ui.AddInspection(NewStoreESSettingsInspection(ui.writer, ui.upgradeUtils, ui.fileUtils, ui.timeout))
	ui.AddInspection(NewDisableShardingInspection(ui.writer, ui.upgradeUtils))
	ui.AddInspection(NewEnableMaintenanceInspection(ui.writer, ui.upgradeUtils, ui.timeout))
}

func (ui *UpgradeInspectorV4) ShowInspectionList() {
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

func (ui *UpgradeInspectorV4) RunExitAction() error {
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

func (ui *UpgradeInspectorV4) handleError(errArray []error) {
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

func (ui *UpgradeInspectorV4) printContactSupport() {
	ui.writer.Println("Please resolve this and try again.")
	ui.writer.Println("Please contact support if you are not sure how to resolve this.")
}

func (ui *UpgradeInspectorV4) RunUpgradeInspector(osDestDir string, skipStorageCheck bool) (isError bool) {
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
	return false
}
