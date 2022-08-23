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
	flagMap, err := u.storage.GetUpgradeFlags()
	if err != nil {
		logrus.Errorf("Unable to get the status of upgrade flags")
		return errors.Wrapf(err, "Unable to get the status of upgrade flags")
	}
	err = u.cerealInterface.EnqueueWorkflowUpgrade(flagMap[pgdb.ControlIndexFlag])
	if err != nil {
		return errors.Wrapf(err, "Unable to enqueue the message in the flow for daily latest flag")
	}

	return nil
}
