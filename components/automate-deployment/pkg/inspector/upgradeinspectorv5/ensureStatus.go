package upgradeinspectorv5

import (
	"errors"
	"fmt"

	"github.com/chef/automate/components/automate-deployment/pkg/cli"
	"github.com/chef/automate/components/automate-deployment/pkg/inspector"
	"github.com/fatih/color"
)

type EnsureStatusInspection struct {
	writer          *cli.Writer
	upgradeUtils    UpgradeV5Utils
	exitError       error
	exitedWithError bool
}

func NewEnsureStatusInspection(w *cli.Writer, upgradeUtils UpgradeV5Utils) *EnsureStatusInspection {
	return &EnsureStatusInspection{
		writer:       w,
		upgradeUtils: upgradeUtils,
	}
}

func (es *EnsureStatusInspection) ShowInfo(index *int) error {
	return nil
}
func (es *EnsureStatusInspection) Skip() {}

func (es *EnsureStatusInspection) GetShortInfo() []string {
	return nil
}

func (es *EnsureStatusInspection) Inspect() (err error) {
	status, err := es.upgradeUtils.GetServicesStatus()
	if err != nil {
		es.setExitError(err)
		return err
	}
	if !status {
		err = errors.New("Please make sure all services are healthy by running " + color.New(color.Bold).Sprint("chef-automate status"))
		es.setExitError(err)
		return err
	}
	return nil
}

func (es *EnsureStatusInspection) setExitError(err error) {
	es.exitError = err
	es.exitedWithError = true
}

func (es *EnsureStatusInspection) GetInstallationType() inspector.InstallationType {
	return inspector.BOTH
}

func (es *EnsureStatusInspection) ExitHandler() error {
	if es.exitedWithError {
		es.writer.Println(fmt.Errorf("["+color.New(color.FgRed).Sprint("Error")+"] %w", es.exitError).Error())
	}
	return nil
}
