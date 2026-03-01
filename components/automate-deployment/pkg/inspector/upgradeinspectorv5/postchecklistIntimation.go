package upgradeinspectorv5

import (
	"fmt"

	"github.com/chef/automate/components/automate-cli/pkg/status"
	"github.com/chef/automate/components/automate-deployment/pkg/cli"
)

type PostChecklistIntimationCheckInspection struct {
	writer *cli.Writer
}

func (pd *PostChecklistIntimationCheckInspection) ShowInfo(index *int) error {
	res, err := pd.writer.Confirm(fmt.Sprintf("%d. After this upgrade completes, you will have to run Post upgrade steps to ensure your data is migrated and your Automate is ready for use", *index))
	if err != nil {
		return status.Errorf(status.InvalidCommandArgsError, err.Error())
	}
	if !res {
		return status.New(status.InvalidCommandArgsError, postChecklistIntimationError)
	}
	*index++
	return nil
}

func NewPostChecklistIntimationCheckInspection(w *cli.Writer) *PostChecklistIntimationCheckInspection {
	return &PostChecklistIntimationCheckInspection{
		writer: w,
	}
}
