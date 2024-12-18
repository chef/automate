package upgradeinspectorv5

import (
	"fmt"

	"github.com/chef/automate/components/automate-cli/pkg/status"
	"github.com/chef/automate/components/automate-deployment/pkg/cli"
)

type PlannedDownTimeInspection struct {
	writer *cli.Writer
}

func (pd *PlannedDownTimeInspection) ShowInfo(index *int) error {
	res, err := pd.writer.Confirm(fmt.Sprintf("%d. You have scheduled downtime for the duration of the upgrade.", *index))
	if err != nil {
		pd.writer.Error(err.Error())
		return status.Errorf(status.InvalidCommandArgsError, err.Error())
	}
	if !res {
		pd.writer.Error(downTimeError)
		return status.New(status.InvalidCommandArgsError, downTimeError)
	}
	*index++
	return nil
}

func NewPlannedDownTimeInspection(w *cli.Writer) *PlannedDownTimeInspection {
	return &PlannedDownTimeInspection{
		writer: w,
	}
}
