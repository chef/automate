package migratorv4

import (
	"fmt"
	"time"

	"github.com/briandowns/spinner"
	"github.com/chef/automate/components/automate-deployment/pkg/cli"
	"github.com/fatih/color"
)

type WaitForHealthy struct {
	writer         *cli.Writer
	utils          MigratorV4Utils
	spinner        *spinner.Spinner
	healthStatus   *bool
	spinnerTimeout time.Duration
}

func NewWaitForHealthy(writer *cli.Writer, utils MigratorV4Utils, healthStatus *bool, spinnerTimeout time.Duration) *WaitForHealthy {
	return &WaitForHealthy{
		writer:         writer,
		utils:          utils,
		healthStatus:   healthStatus,
		spinnerTimeout: spinnerTimeout,
	}
}

func (wfh *WaitForHealthy) Run() error {
	return nil
}

func (wfh *WaitForHealthy) ErrorHandler() {
	return
}

func (wfh *WaitForHealthy) showErrorStatus() {
	wfh.spinner.FinalMSG = SPACES_BEFORE_STEPS + " " + color.New(color.FgRed).Sprint("✖") + "  Chef Automate status is unhealthy"
	wfh.spinner.Stop()
	wfh.writer.Println("")
}

func (wfh *WaitForHealthy) showSuccessStatus() {
	wfh.spinner.FinalMSG = SPACES_BEFORE_STEPS + " " + color.New(color.FgGreen).Sprint("✔") + "  Chef Automate status is healthy"
	wfh.spinner.Stop()
	wfh.writer.Println("")
}

func (wfh *WaitForHealthy) showStartStatus() {
	wfh.spinner = wfh.writer.NewSpinnerWithTab()
	wfh.spinner.Suffix = fmt.Sprintf("  Checking Chef automate status")
	wfh.spinner.Start()
	time.Sleep(wfh.spinnerTimeout)
}

func (wfh *WaitForHealthy) DefferedHandler() error {
	if *wfh.healthStatus {
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
	}
	return nil
}
