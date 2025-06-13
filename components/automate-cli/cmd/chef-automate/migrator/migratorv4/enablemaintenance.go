package migratorv4

import (
	"fmt"
	"time"

	"github.com/briandowns/spinner"
	"github.com/chef/automate/components/automate-deployment/pkg/cli"
	"github.com/chef/automate/lib/io/fileutils"
	"github.com/fatih/color"
)

type EnableMaintenance struct {
	writer         *cli.Writer
	utils          MigratorV4Utils
	fileutils      fileutils.FileUtils
	spinner        *spinner.Spinner
	runError       error
	hasError       bool
	isExecuted     bool
	spinnerTimeout time.Duration
	timeout        int64
}

func NewEnableMaintenance(w *cli.Writer, utils MigratorV4Utils, timeout int64, spinnerTimeout time.Duration) *EnableMaintenance {
	return &EnableMaintenance{
		writer:         w,
		utils:          utils,
		timeout:        timeout,
		spinnerTimeout: spinnerTimeout,
	}
}

func (em *EnableMaintenance) Run() (err error) {
	em.showTurningOn()
	_, _, err = em.utils.SetMaintenanceMode(em.timeout, true)
	if err != nil {
		em.showError()
		em.setError(err)
		return err
	}
	em.setExecuted()
	em.showSuccess()
	return nil
}

func (em *EnableMaintenance) showTurningOn() {
	em.spinner = em.writer.NewSpinnerWithTab()
	em.spinner.Suffix = fmt.Sprintf("  Turning ON maintenance mode")
	em.spinner.Start()
	time.Sleep(em.spinnerTimeout)
}

func (em *EnableMaintenance) showSuccess() {
	em.spinner.FinalMSG = SPACES_BEFORE_STEPS + " " + fmt.Sprint(color.New(color.FgGreen).Sprint("✔")+"  Maintenance mode turned ON successfully")
	em.spinner.Stop()
	em.writer.Println("")
}

func (em *EnableMaintenance) showError() {
	em.spinner.FinalMSG = SPACES_BEFORE_STEPS + " " + color.New(color.FgRed).Sprint("✖") + "  Failed to turn maintenance mode ON"
	em.spinner.Stop()
	em.writer.Println("")
}

func (em *EnableMaintenance) showTurningOff() {
	em.spinner = em.writer.NewSpinnerWithTab()
	em.spinner.Suffix = fmt.Sprintf("  Turning OFF maintenance mode")
	em.spinner.Start()
	time.Sleep(em.spinnerTimeout)
}

func (em *EnableMaintenance) showOffSuccess() {
	em.spinner.FinalMSG = SPACES_BEFORE_STEPS + " " + fmt.Sprint(color.New(color.FgGreen).Sprint("✔")+"  Maintenance mode turned OFF successfully")
	em.spinner.Stop()
	em.writer.Println("")
}

func (em *EnableMaintenance) showOffError() {
	em.spinner.FinalMSG = SPACES_BEFORE_STEPS + " " + color.New(color.FgRed).Sprint("✖") + "  Failed to turn maintenance mode OFF"
	em.spinner.Stop()
	em.writer.Println("")
}

func (em *EnableMaintenance) setExecuted() {
	em.isExecuted = true
}

func (em *EnableMaintenance) OnSuccess() (err error) {
	em.showTurningOff()
	_, _, err = em.utils.SetMaintenanceMode(em.timeout, false)
	if err != nil {
		em.showOffError()
		em.ErrorHandler()
		return err
	}
	em.showOffSuccess()
	return nil
}

func (em *EnableMaintenance) setError(err error) error {
	em.runError = err
	em.hasError = true
	return err
}

func (em *EnableMaintenance) ErrorHandler() {
	if em.hasError {
		em.writer.Println("[" + color.New(color.FgRed).Sprint("Error") + "] " + em.runError.Error())
	}
}
