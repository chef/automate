package migratorV4

import "github.com/chef/automate/components/automate-deployment/pkg/cli"

type AutomateStop struct {
	writer *cli.Writer
	utils  MigratorV4Utils
}

func NewAutomateStop(w *cli.Writer, utils MigratorV4Utils) *AutomateStop {
	return &AutomateStop{
		writer: w,
		utils:  utils,
	}
}

func (as *AutomateStop) Run() error {
	return as.utils.StopAutomate()
}

func (as *AutomateStop) Skip() error {
	return nil
}

func (as *AutomateStop) ExitHandler() error {
	return as.utils.StartAutomate()
}
