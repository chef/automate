package migratorV4

import (
	"fmt"
	"time"

	"github.com/briandowns/spinner"
	"github.com/chef/automate/components/automate-deployment/pkg/cli"
	"github.com/fatih/color"
)

const (
	HABROOT_CMD = "HAB_LICENSE=accept-no-persist hab pkg path chef/deployment-service"
	FSCRIPT     = `
mv %[1]vsvc/automate-opensearch/data %[1]vsvc/automate-opensearch/data.os; 
mv %[1]vsvc/automate-opensearch/var %[1]vsvc/automate-opensearch/var.os; 
cp -r %[1]vsvc/automate-elasticsearch/data %[1]vsvc/automate-opensearch/; 
cp -r %[1]vsvc/automate-elasticsearch/var %[1]vsvc/automate-opensearch/; 
chown -RL hab:hab %[1]vsvc/automate-opensearch/data; 
chown -RL hab:hab %[1]vsvc/automate-opensearch/var;`
)

type MigrationScript struct {
	writer     *cli.Writer
	utils      MigratorV4Utils
	spinner    *spinner.Spinner
	runError   error
	hasError   bool
	isExecuted bool
}

func NewMigrationScript(w *cli.Writer, utils MigratorV4Utils) *MigrationScript {
	return &MigrationScript{
		writer: w,
		utils:  utils,
	}
}

func (ms *MigrationScript) setError(err error) error {
	ms.runError = err
	ms.hasError = true
	return err
}

func (ms *MigrationScript) showCopyError() {
	ms.spinner.FinalMSG = " " + color.New(color.FgRed).Sprint("✖") + "  Failed to copy data"
	ms.spinner.Stop()
	ms.writer.Println("")
}

func (ms *MigrationScript) showCopied() {
	ms.spinner.FinalMSG = " " + color.New(color.FgGreen).Sprint("✔") + "  Data Copied Successfully"
	ms.spinner.Stop()
	ms.writer.Println("")
}

func (ms *MigrationScript) showCopying() {
	ms.spinner = ms.writer.NewSpinner()
	ms.spinner.Suffix = fmt.Sprintf("  Copying Data")
	ms.spinner.Start()
	time.Sleep(time.Second)
}

func (ms *MigrationScript) Run() error {
	ms.showCopying()
	habRoot := ms.utils.GetHabRootPath(HABROOT_CMD)
	script := fmt.Sprintf(FSCRIPT, habRoot)
	err := ms.utils.ExecShCommand(script)
	if err != nil {
		ms.showCopyError()
		ms.setError(err)
		return err
	}
	ms.showCopied()
	return nil
}

func (ms *MigrationScript) Skip() error {
	return nil
}

func (ms *MigrationScript) ErrorHandler() {
	if ms.hasError {
		ms.writer.Println(ms.runError.Error())
	}
}
