package migratorv4

import (
	"fmt"
	"time"

	"github.com/briandowns/spinner"
	"github.com/chef/automate/components/automate-deployment/pkg/cli"
	"github.com/fatih/color"
)

type AutomateStop struct {
	writer       *cli.Writer
	utils        MigratorV4Utils
	spinner      *spinner.Spinner
	runError     error
	hasError     bool
	isExecuted   bool
	healthStatus *bool
}

func NewAutomateStop(w *cli.Writer, utils MigratorV4Utils, healthStatus *bool) *AutomateStop {
	return &AutomateStop{
		writer:       w,
		utils:        utils,
		healthStatus: healthStatus,
	}
}

func (as *AutomateStop) Run() (err error) {
	as.showStopping()
	err = as.utils.StopAutomate()
	if err != nil {
		as.showStopError()
		return as.setError(err)
	}
	as.isExecuted = true
	as.showStopped()
	return nil
}

func (as *AutomateStop) setError(err error) error {
	as.runError = err
	as.hasError = true
	return err
}

func (as *AutomateStop) showStopError() {
	as.spinner.FinalMSG = SPACES_BEFORE_STEPS + " " + color.New(color.FgRed).Sprint("✖") + "  Failed to stop Chef Automate"
	as.spinner.Stop()
	as.writer.Println("")
}

func (as *AutomateStop) showStopped() {
	as.spinner.FinalMSG = SPACES_BEFORE_STEPS + " " + color.New(color.FgGreen).Sprint("✔") + "  Chef Automate Stopped"
	as.spinner.Stop()
	as.writer.Println("")
}

func (as *AutomateStop) showStopping() {
	as.spinner = as.writer.NewSpinnerWithTab()
	as.spinner.Suffix = fmt.Sprintf("  Stopping Chef Automate")
	as.spinner.Start()
	time.Sleep(time.Second)
}

func (as *AutomateStop) showStartError() {
	as.spinner.FinalMSG = SPACES_BEFORE_STEPS + " " + color.New(color.FgRed).Sprint("✖") + "  Failed to start Chef Automate"
	as.spinner.Stop()
	as.writer.Println("")
}

func (as *AutomateStop) showStarted() {
	as.spinner.FinalMSG = SPACES_BEFORE_STEPS + " " + color.New(color.FgGreen).Sprint("✔") + "  Chef Automate Started"
	as.spinner.Stop()
	as.writer.Println("")
}

func (as *AutomateStop) showStarting() {
	as.spinner = as.writer.NewSpinnerWithTab()
	as.spinner.Suffix = fmt.Sprintf("  Restarting Chef Automate")
	as.spinner.Start()
	time.Sleep(time.Second)
}

func (as *AutomateStop) DefferedHandler() error {
	if as.isExecuted {
		as.showStarting()
		err := as.utils.StartAutomate()
		if err != nil {
			as.showStartError()
			return err
		}
		as.showStarted()
		*as.healthStatus = true
	}
	return nil
}

func (as *AutomateStop) ErrorHandler() {
	if as.hasError {
		as.writer.Println("[" + color.New(color.FgRed).Sprint("Error") + "] " + as.runError.Error())
	}
}
