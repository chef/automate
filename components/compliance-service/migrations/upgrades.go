package migrations

import (
	"context"
	"fmt"
	"github.com/chef/automate/components/compliance-service/dao/pgdb"
	"github.com/chef/automate/lib/cereal"
	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"
)

type Upgrades struct {
	DB            *pgdb.DB
	cerealManager *cereal.Manager
}

func New(db *pgdb.DB, cerealManager *cereal.Manager) *Upgrades {
	return &Upgrades{db, cerealManager}
}

const getUpgradeValueForDayLatest = `
Select upgrade_value from upgrade_flags where upgrade_flag='day_latest'; `

const UpdateQueryForDayLatest = `
Update upgrade_flags set upgrade_value=true where upgrade_flag='day_latest';`

//PollForUpgradeFlagDayLatest checks for the day latest flag value in upgrade flags
func (u *Upgrades) PollForUpgradeFlagDayLatest() error {
	var status bool
	var err error
	status, err = u.GetDayLatestUpgradeFlagValue()
	if !status {
		err = u.cerealManager.EnqueueWorkflow(context.TODO(), MigrationWorkflowName,
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

//UpdateDayLatestFlagToTrue updates the day latest flags to true
func (u *Upgrades) UpdateDayLatestFlagToTrue() error {
	_, err := u.DB.Exec(UpdateQueryForDayLatest)
	if err != nil {
		return errors.Wrapf(err, "Unable to UpdateDayLatestFlagToTrue")
	}
	return nil
}

func (u *Upgrades) GetDayLatestUpgradeFlagValue() (status bool, err error) {
	logrus.Info("Inside the get day latest flag")
	err = u.DB.QueryRow(getUpgradeValueForDayLatest).Scan(&status)
	if err != nil {
		return status, errors.Wrapf(err, "PollForUpgradeFlagDayLatest unable to get current job status")
	}
	return status, err
}
