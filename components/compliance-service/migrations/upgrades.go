package migrations

import (
	"context"
	"fmt"
	"github.com/chef/automate/lib/cereal"
	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"
)

type Upgrade struct {
	UpgradesDB   *UpgradesDB
	cerealManger *cereal.Manager
}

func NewService(db *UpgradesDB, cerealManger *cereal.Manager) *Upgrade {
	return &Upgrade{UpgradesDB: db, cerealManger: cerealManger}
}

//PollForUpgradeFlagDayLatest checks for the day latest flag value in upgrade flags
func (u *Upgrade) PollForUpgradeFlagDayLatest() error {
	var status bool
	var err error
	status, err = u.UpgradesDB.GetDayLatestUpgradeFlagValue()
	if !status {
		err = u.cerealManger.EnqueueWorkflow(context.TODO(), MigrationWorkflowName,
			fmt.Sprintf("%s-%s", MigrationWorkflowName, DayLatestMigrationTaskName),
			MigrationWorkflowParameters{
				DayLatestFlag: status,
			})
		if err != nil {
			logrus.Debugf("Unable to Enqueue Workflow for Day Latest Task")
			return errors.Wrapf(err, "Unable to Enqueue Workflow for Day Latest Task")
		}
	}
	return nil
}
