package upgradeinspectorv4

import (
	"strings"

	"github.com/pkg/errors"

	"github.com/chef/automate/components/automate-deployment/pkg/cli"
	"github.com/chef/automate/components/automate-deployment/pkg/inspector"
)

type UpgradeInspectorV4 struct {
	writer      *cli.Writer
	inspections []inspector.Inspection
	osDestDir   string
}

const (
	HAB_DIR string = "/hab"
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
	if len(ui.osDestDir) == 0 || ui.osDestDir == HAB_DIR {
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
		return errors.New("Upgrade process terminated.")
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
			err = inspection.(inspector.SystemInspection).Inspect()
			if err != nil {
				return errors.Wrap(err, "Upgrade process terminated.")
			}
		}
	}
	return nil
}

func NewUpgradeInspectorV4(w *cli.Writer) inspector.Inspector {
	return &UpgradeInspectorV4{
		writer: w,
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
