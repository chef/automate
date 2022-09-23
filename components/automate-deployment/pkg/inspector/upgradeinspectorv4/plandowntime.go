package upgradeinspectorv4

import (
	"github.com/chef/automate/components/automate-deployment/pkg/cli"
)

type PlannedDownTimeInspection struct {
	writer          *cli.Writer
	exitedWithError bool
	exitError       error
}

func (pd *PlannedDownTimeInspection) ShowInfo(index *int) error {
	pd.writer.Printf("%d. You have planned downtime\n", *index)
	return nil
}

func NewPlannedDownTimeInspection(w *cli.Writer) *PlannedDownTimeInspection {
	return &PlannedDownTimeInspection{
		writer: w,
	}
}

func (pd *PlannedDownTimeInspection) PrintExitMessage() error {
	return nil
}

func (pd *PlannedDownTimeInspection) HasExitedWithError() bool {
	return pd.exitedWithError
}

func (pd *PlannedDownTimeInspection) SetExitedWithError(status bool) {
	pd.exitedWithError = status
}
