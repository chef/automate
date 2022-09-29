package migratorv4

import (
	"fmt"
	"time"

	"github.com/briandowns/spinner"
	"github.com/chef/automate/components/automate-deployment/pkg/cli"
	"github.com/chef/automate/lib/io/fileutils"
	"github.com/fatih/color"
	"github.com/pkg/errors"
)

const (
	FSCRIPT = `
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
	fileutils  fileutils.FileUtils
	spinner    *spinner.Spinner
	runError   error
	hasError   bool
	isExecuted bool
}

func NewMigrationScript(w *cli.Writer, utils MigratorV4Utils, fileutils fileutils.FileUtils) *MigrationScript {
	return &MigrationScript{
		writer:    w,
		utils:     utils,
		fileutils: fileutils,
	}
}

func (ms *MigrationScript) setError(err error) error {
	ms.runError = err
	ms.hasError = true
	return err
}

func (ms *MigrationScript) showCopyError() {
	ms.spinner.FinalMSG = SPACES_BEFORE_STEPS + " " + color.New(color.FgRed).Sprint("✖") + "  Failed to copy data"
	ms.spinner.Stop()
	ms.writer.Println("")
}

func (ms *MigrationScript) showCopied() {
	ms.spinner.FinalMSG = SPACES_BEFORE_STEPS + " " + color.New(color.FgGreen).Sprint("✔") + "  Data Copied Successfully"
	ms.spinner.Stop()
	ms.writer.Println("")
}

func (ms *MigrationScript) showCopying() {
	ms.spinner = ms.writer.NewSpinnerWithTab()
	ms.spinner.Suffix = fmt.Sprintf("  Copying Data")
	ms.spinner.Start()
	time.Sleep(time.Second)
}

func (ms *MigrationScript) Run() error {
	ms.showCopying()
	habRoot := ms.fileutils.GetHabRootPath()
	script := fmt.Sprintf(FSCRIPT, habRoot)
	args := []string{
		"-c",
		script,
	}
	err := ms.utils.ExecuteCommand("/bin/sh", args, "")
	if err != nil {
		err = errors.Wrap(err, "Failed to copy data while migration")
		ms.showCopyError()
		ms.setError(err)
		return err
	}
	ms.showCopied()
	return nil
}

func (ms *MigrationScript) ErrorHandler() {
	if ms.hasError {
		ms.writer.Println("[" + color.New(color.FgRed).Sprint("Error") + "] " + ms.runError.Error())
	}
}
