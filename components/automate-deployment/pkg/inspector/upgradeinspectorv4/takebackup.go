package upgradeinspectorv4

import (
	"github.com/chef/automate/components/automate-deployment/pkg/cli"
	"github.com/fatih/color"
)

type TakeBackupInspection struct {
	writer          *cli.Writer
	exitedWithError bool
	exitError       error
}

func (tb *TakeBackupInspection) ShowInfo(index *int) error {
	tb.writer.Printf("%d. You have taken backup by running command: "+
		color.New(color.Bold).Sprint("chef automate backup create")+"\n", *index)
	return nil
}

func NewTakeBackupInspection(w *cli.Writer) *TakeBackupInspection {
	return &TakeBackupInspection{
		writer: w,
	}
}

func (tb *TakeBackupInspection) PrintExitMessage() error {
	return nil
}

func (tb *TakeBackupInspection) HasExitedWithError() bool {
	return tb.exitedWithError
}

func (tb *TakeBackupInspection) SetExitedWithError(status bool) {
	tb.exitedWithError = status
}
