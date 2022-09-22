package upgradeinspectorv4

import (
	"github.com/chef/automate/components/automate-deployment/pkg/cli"
	"github.com/chef/automate/components/automate-deployment/pkg/inspector"
)

type PlannedDownTimeInspection struct {
	writer *cli.Writer
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

func (pd *PlannedDownTimeInspection) GetInstallationType() inspector.InstallationType {
	return inspector.BOTH
}
