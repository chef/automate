package upgradeinspectorv4

import (
	"strings"

	"github.com/pkg/errors"

	"github.com/chef/automate/components/automate-deployment/pkg/cli"
	"github.com/chef/automate/components/automate-deployment/pkg/inspector"
	"github.com/chef/automate/lib/io/fileutils"
)

type UpgradeInspectorV4 struct {
	writer       *cli.Writer
	inspections  []inspector.Inspection
	osDestDir    string
	upgradeUtils UpgradeV4Utils
	fileUtils    fileutils.FileUtils
	timeout      int64
	isExternal   bool
}

const (
	HAB_DIR         string = "/hab"
	USER_TERMINATED string = "Upgrade process terminated."
)

func (ui *UpgradeInspectorV4) ShowInfo() error {
	ui.writer.Println(`This is a major upgrade!
In this release, Elasticsearch will be migrated to OpenSearch.
`)
	if len(ui.inspections) > 0 {
		ui.writer.Println("Please make sure following things are taken care of")
	}
	index := 1
	for _, inspection := range ui.inspections {
		err := inspection.ShowInfo(&index)
		if err != nil {
			return err
		}
		index++
	}
	if len(ui.inspections) > 0 {
		ui.writer.Println("")
	}

	if len(ui.osDestDir) == 0 || ui.osDestDir == HAB_DIR || ui.upgradeUtils.IsExternalElasticSearch() {
		ui.showOSDestDirFlagMsg()
	}
	ui.writer.Println(`For more information, visit 
https://docs.chef.io/automate/major_upgrade 4.x/
`)
	return ui.promptToContinue()
}

func (ui *UpgradeInspectorV4) promptToContinue() error {
	isContinue, err := ui.writer.Confirm("Would you like to proceed with the upgrade?")
	if err != nil {
		return err
	}
	if !isContinue {
		return errors.New(USER_TERMINATED)
	}
	return nil
}

func (ui *UpgradeInspectorV4) showOSDestDirFlagMsg() {
	ui.writer.Println(`You can always change the OpenSearch destination directory by using the flag:
  $ chef-automate upgrade run --major --os-dest-data-dir <path to new directory>
`)
}

func (ui *UpgradeInspectorV4) Inspect() (err error) {
	ui.writer.Println("Pre flight checks")
	for _, inspection := range ui.inspections {
		var i interface{} = inspection
		_, ok := i.(inspector.SystemInspection)
		if ok {
			installationType := inspection.(inspector.SystemInspection).GetInstallationType()
			if ui.isExternal && (installationType == inspector.EXTERNAL || installationType == inspector.BOTH) {
				if err != nil {
					inspection.(inspector.SystemInspection).Skip()
				} else {
					err = inspection.(inspector.SystemInspection).Inspect()
				}
			} else if !ui.isExternal && (installationType == inspector.EMBEDDED || installationType == inspector.BOTH) {
				if err != nil {
					inspection.(inspector.SystemInspection).Skip()
				} else {
					err = inspection.(inspector.SystemInspection).Inspect()
				}
			}
		}
	}
	if err != nil {
		return errors.Wrap(err, USER_TERMINATED)
	}
	return nil
}

func (ui *UpgradeInspectorV4) PreExit() (err error) {
	for _, inspection := range ui.inspections {
		var i interface{} = inspection
		_, ok := i.(inspector.ExitInspection)
		if ok {
			err = inspection.(inspector.ExitInspection).PreExit()
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
		isExternal:   upgradeUtils.IsExternalElasticSearch(),
	}
}

func (ui *UpgradeInspectorV4) SetOSDestDir(path string) {
	if strings.TrimSpace(path) != "" {
		ui.osDestDir = path
	}
}

func (ui *UpgradeInspectorV4) AddInspection(inspection inspector.Inspection) {
	if inspection != nil {
		ui.inspections = append(ui.inspections, inspection)
	}
}

func (ui *UpgradeInspectorV4) AddDefaultInspections() {
	ui.AddInspection(NewPlannedDownTimeInspection(ui.writer))
	ui.AddInspection(NewTakeBackupInspection(ui.writer))
	diskSpaceInspection := NewDiskSpaceInspection(ui.writer, ui.upgradeUtils.IsExternalElasticSearch(), ui.osDestDir, ui.fileUtils)
	ui.AddInspection(diskSpaceInspection)
	esBasePath := ui.upgradeUtils.GetESBasePath(ui.timeout)
	ui.AddInspection(NewESIndexInspection(ui.writer, ui.upgradeUtils, esBasePath))
	ui.AddInspection(NewDisableShardingInspection(ui.writer, ui.upgradeUtils))
	ui.AddInspection(NewReplaceS3UrlInspection(ui.writer, ui.upgradeUtils))
}

func (ui *UpgradeInspectorV4) ShowInspectionList() {
	ui.writer.Println("\nFollowing Pre-flight checks will be conducted")
	index := 1
	for _, inspection := range ui.inspections {
		var i interface{} = inspection
		_, ok := i.(inspector.SystemInspection)
		if ok {
			msgs := inspection.(inspector.SystemInspection).GetShortInfo()
			for _, msg := range msgs {
				if msg != "" {
					ui.writer.Printf("%d. %s\n", index, msg)
					index++
				}
			}
		}
	}
	ui.writer.Println("")
}
