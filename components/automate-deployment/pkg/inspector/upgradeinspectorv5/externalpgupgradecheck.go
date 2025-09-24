package upgradeinspectorv5

import (
	"fmt"

	"github.com/chef/automate/components/automate-cli/pkg/status"
	"github.com/chef/automate/components/automate-deployment/pkg/cli"
)

type ExternalPGUpgradeCheckInspection struct {
	writer       *cli.Writer
	isExternalPG bool
}

func (pd *ExternalPGUpgradeCheckInspection) ShowInfo(index *int) error {
	if pd.isExternalPG {
		res, err := pd.writer.Confirm(fmt.Sprintf("%d. Upgrade your PostgreSQL 13.5 to 17.0 with the help of your Database Administrator", *index))
		if err != nil {
			return status.Errorf(status.InvalidCommandArgsError, err.Error())
		}
		if !res {
			return status.New(status.InvalidCommandArgsError, downTimeError)
		}
		*index++
	}
	return nil
}

func NewExternalPGUpgradeCheckInspection(w *cli.Writer, isExternalPG bool) *ExternalPGUpgradeCheckInspection {
	return &ExternalPGUpgradeCheckInspection{
		writer:       w,
		isExternalPG: isExternalPG,
	}
}
