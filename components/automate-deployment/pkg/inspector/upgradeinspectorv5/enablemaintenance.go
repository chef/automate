package upgradeinspectorv5

import (
	"fmt"
	"time"

	"github.com/briandowns/spinner"
	"github.com/chef/automate/components/automate-deployment/pkg/cli"
	"github.com/chef/automate/components/automate-deployment/pkg/inspector"
	"github.com/fatih/color"
)

type EnableMaintenanceInspection struct {
	writer          *cli.Writer
	upgradeUtils    UpgradeV5Utils
	timeout         int64
	isExecuted      bool
	spinner         *spinner.Spinner
	exitError       error
	exitedWithError bool
}

func NewEnableMaintenanceInspection(w *cli.Writer, utls UpgradeV5Utils, timeout int64) *EnableMaintenanceInspection {
	return &EnableMaintenanceInspection{
		writer:       w,
		upgradeUtils: utls,
		timeout:      timeout,
	}
}

func (em *EnableMaintenanceInspection) ShowInfo(index *int) error {
	return nil
}
func (em *EnableMaintenanceInspection) Skip() {
}
func (em *EnableMaintenanceInspection) GetShortInfo() []string {
	return nil
}

func (em *EnableMaintenanceInspection) Inspect() (err error) {
	em.showTurningOn()
	isMaintenanceOn, err := em.upgradeUtils.GetMaintenanceStatus(em.timeout)
	if err != nil {
		em.showError()
		em.setExitError(err)
		return err
	}
	if isMaintenanceOn {
		em.showSuccess()
		return nil
	}
	_, _, err = em.upgradeUtils.SetMaintenanceMode(em.timeout, true)
	if err != nil {
		em.showError()
		em.setExitError(err)
		return err
	}
	em.setExecuted()
	em.showSuccess()
	return nil
}

func (em *EnableMaintenanceInspection) setExitError(err error) {
	em.exitedWithError = true
	em.exitError = err
}

func (em *EnableMaintenanceInspection) showTurningOn() {
	em.spinner = em.writer.NewSpinner()
	em.spinner.Suffix = "  Turning ON maintenance mode"
	em.spinner.Start()
	time.Sleep(time.Second)
}

func (em *EnableMaintenanceInspection) showSuccess() {
	em.spinner.FinalMSG = "\n " + fmt.Sprintf(color.New(color.FgGreen).Sprint("✔")+"  Maintenance mode turned ON successfully\n")
	em.spinner.Stop()
	em.writer.Println("")
}

func (em *EnableMaintenanceInspection) showError() {
	em.spinner.FinalMSG = " " + color.New(color.FgRed).Sprint("✖") + "  Failed to turn maintenance mode ON"
	em.spinner.Stop()
	em.writer.Println("")
}

func (em *EnableMaintenanceInspection) setExecuted() {
	em.isExecuted = true
}

func (em *EnableMaintenanceInspection) RollBackHandler() (err error) {
	if !em.isExecuted {
		return nil
	}
	isMaintenanceOn, err := em.upgradeUtils.GetMaintenanceStatus(em.timeout)
	if err != nil {
		return err
	}
	if !isMaintenanceOn {
		return nil
	}
	_, _, err = em.upgradeUtils.SetMaintenanceMode(em.timeout, false)
	if err != nil {
		return err
	}
	return nil
}

func (em *EnableMaintenanceInspection) GetInstallationType() inspector.InstallationType {
	return inspector.BOTH
}

func (em *EnableMaintenanceInspection) ExitHandler() error {
	if em.exitedWithError {
		em.writer.Println(fmt.Errorf("["+color.New(color.FgRed).Sprint("Error")+"] %w", em.exitError).Error())
	}
	return nil
}
