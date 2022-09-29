package migratorV4

import (
	"fmt"
	"time"

	"github.com/briandowns/spinner"
	"github.com/chef/automate/components/automate-deployment/pkg/cli"
	"github.com/fatih/color"
)

type WaitForHealthy struct {
	writer  *cli.Writer
	utils   MigratorV4Utils
	spinner *spinner.Spinner
}

func NewWaitForHealthy(writer *cli.Writer, utils MigratorV4Utils) *WaitForHealthy {
	return &WaitForHealthy{
		writer: writer,
		utils:  utils,
	}
}

func (wfh *WaitForHealthy) Run() error {
	return nil
}
func (wfh *WaitForHealthy) Skip() error {
	return nil
}

func (wfh *WaitForHealthy) ErrorHandler() {
	return
}

func (wfh *WaitForHealthy) showErrorStatus() {
	wfh.spinner.FinalMSG = " " + color.New(color.FgRed).Sprint("✖") + "  Chef Automate status is unhealthy"
	wfh.spinner.Stop()
	wfh.writer.Println("")
}

func (wfh *WaitForHealthy) showSuccessStatus() {
	wfh.spinner.FinalMSG = " " + color.New(color.FgGreen).Sprint("✔") + "  Chef Automate status is healthy"
	wfh.spinner.Stop()
	wfh.writer.Println("")
}

func (wfh *WaitForHealthy) showStartStatus() {
	wfh.spinner = wfh.writer.NewSpinner()
	wfh.spinner.Suffix = fmt.Sprintf("  Checking Chef automate status")
	wfh.spinner.Start()
	time.Sleep(time.Second)
}

func (wfh *WaitForHealthy) DefferedHandler() error {
	wfh.showStartStatus()
	args := []string{
		"status",
		"--wait-for-healthy",
	}
	err := wfh.utils.ExecuteCommand("chef-automate", args, "")
	if err != nil {
		wfh.showErrorStatus()
		return err
	}
	wfh.showSuccessStatus()
	return nil
}
