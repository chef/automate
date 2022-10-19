package migrations

import (
	"time"

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
func (u *Upgrade) PollForUpgradeFlagDayLatest(upgradeDate time.Time) error {
	logrus.Infof("upgrade will run from %s to till now", upgradeDate.String())
	controlFlag, err := u.getUpgradeFlag(pgdb.ControlIndexFlag)
	if err != nil {
		return err
	}
	if controlFlag.Status {
		err = u.cerealInterface.EnqueueWorkflowUpgrade(upgradeDate)
		if err != nil {
			return errors.Wrapf(err, "Unable to enqueue the message in the flow for daily latest flag")
		}
		u.storage.UpdateControlFlagTimeStamp()
	}

	return nil
}

func (u *Upgrade) getUpgradeFlag(flagName string) (*pgdb.Flag, error) {
	flagMap, err := u.storage.GetUpgradeFlags()
	if err != nil {
		logrus.Errorf("Unable to get the status of upgrade flags")
		return nil, errors.Wrapf(err, "Unable to get the status of upgrade flags")
	}
	if flag, ok := flagMap[flagName]; ok {
		return &flag, nil
	}
	return nil, nil
}

// UpdateFlags makes the necessary changes to upgrade_flags table based on the received configuration as below
// If isConfigEnabled - true --> returns the existing timestamp(we need to run the upgrades till that day),
// and removes the flag record from DB once the upgrade is successful
// If isConfigEnabled - false and flag entry not available in DB --> create a record for enhanced_reporting with upgrade_time as current time,
// and update the control_index upgrade_value to true(so that when the config enabled, it triggers the upgrade for control index)
// If isConfigEnabled - false and flag entry available in DB --> update the control_index upgrade_value to true
func (u *Upgrade) UpdateFlags(isConfigEnabled bool) (date time.Time, err error) {
	flag, err := u.getUpgradeFlag(pgdb.EnhancedReportingEnabledFlag)
	if err != nil {
		return time.Time{}, err
	}
	if isConfigEnabled {
		if flag == nil {
			return time.Time{}, err
		}
		return flag.UpgradedTime, nil
	}
	// configuration is disabled
	err = u.storage.UpdateControlFlagValue(true)
	if err != nil {
		return time.Time{}, err
	}
	if flag == nil {
		err = u.storage.AddEnhancedReportingFlag()
		if err != nil {
			errors.Wrapf(err, "Unable to delete the enhanced_reporting flag entry")
		}
	}
	return time.Time{}, err
}
