package upgradeinspectorv5

import (
	"fmt"

	"github.com/chef/automate/components/automate-cli/pkg/status"
	"github.com/chef/automate/components/automate-deployment/pkg/cli"
	"github.com/fatih/color"
)

type TakeBackupInspection struct {
	writer *cli.Writer
}

func (tb *TakeBackupInspection) ShowInfo(index *int) error {
	res, err := tb.writer.Confirm(fmt.Sprintf("%d. You have taken a backup by running the command: "+
		color.New(color.Bold).Sprint("chef automate backup create")+".", *index))
	if err != nil {
		return status.Errorf(status.InvalidCommandArgsError, err.Error())
	}
	if !res {
		return status.New(status.InvalidCommandArgsError, backupError)
	}
	*index++
	return nil
}

func NewTakeBackupInspection(w *cli.Writer) *TakeBackupInspection {
	return &TakeBackupInspection{
		writer: w,
	}
}
