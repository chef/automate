package migrations

import (
	"github.com/chef/automate/components/compliance-service/dao/pgdb"
	"github.com/chef/automate/lib/cereal"
	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"
)

type Upgrade struct {
	storage         pgdb.Storage
	cerealInterface cerealInterface
}

func NewService(pg *pgdb.UpgradesDB, cerealManger *cereal.Manager) *Upgrade {
	return &Upgrade{storage: &pgdb.UpgradesDB{DB: pg.DB}, cerealInterface: &cerealService{
		cerealManger: cerealManger,
	}}
}

//PollForUpgradeFlagDayLatest checks for the day latest flag value in upgrade flags
func (u *Upgrade) PollForUpgradeFlagDayLatest() error {

	status, err := u.storage.GetDayLatestUpgradeFlagValue()
	if err != nil {
		logrus.Errorf("Unable to get the status of day latest flag")
		return errors.Wrapf(err, "Unable to get the status of day latest flag")
	}
	err = u.cerealInterface.EnqueueWorkflowDayLatest(status)
	if err != nil {
		return errors.Wrapf(err, "Unable to enqueue the message in the flow for daily latest flag")
	}

	status, err = u.storage.GetControlLatestUpgradeFlagValue()
	if err != nil {
		logrus.Errorf("Unable to get the status of Control index flag")
		return errors.Wrapf(err, "Unable to get the status of control index flag")
	}

	err = u.cerealInterface.EnqueueWorkflowControl(status)
	if err != nil {
		return errors.Wrapf(err, "Unable to enqueue the message in the flow for control index flag")
	}

	return nil
}
