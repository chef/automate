package migratorV4

import "github.com/chef/automate/components/automate-deployment/pkg/cli"

type AutomateStop struct {
	writer   *cli.Writer
	utils    MigratorV4Utils
	runError error
	hasError bool
}

func NewAutomateStop(w *cli.Writer, utils MigratorV4Utils) *AutomateStop {
	return &AutomateStop{
		writer: w,
		utils:  utils,
	}
}

func (as *AutomateStop) Run() (err error) {
	err = as.utils.StopAutomate()
	if err != nil {
		return as.setError(err)

	}
	return nil
}

func (as *AutomateStop) setError(err error) error {
	as.runError = err
	as.hasError = true
	return err
}

func (as *AutomateStop) Skip() error {
	return nil
}

func (as *AutomateStop) DefferedHandler() error {
	return as.utils.StartAutomate()
}

func (as *AutomateStop) ErrorHandler() {
	if as.hasError {
		as.writer.Println(as.runError.Error())
	}
}
