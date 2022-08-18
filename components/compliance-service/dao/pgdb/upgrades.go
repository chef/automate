package pgdb

import (
	"fmt"
	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"
)

type UpgradesDB struct {
	DB *DB
}

func NewDB(db *DB) *UpgradesDB {
	return &UpgradesDB{db}
}

//UpdateDayLatestFlagToTrue updates the day latest flags to true
func (u *UpgradesDB) UpdateDayLatestFlagToFalse() error {
	_, err := u.DB.Exec(getUpdateQuery(DayLatestFlag))
	if err != nil {
		return errors.Wrapf(err, "Unable to UpdateDayLatestFlagToTrue")
	}
	return nil
}

//UpdateControlIndexFlagToTrue updates the day latest flags to true
func (u *UpgradesDB) UpdateControlFlagToFalse() error {
	_, err := u.DB.Exec(getUpdateQuery(ControlIndexFlag))
	if err != nil {
		return errors.Wrapf(err, "Unable to UpdateDayLatestFlagToTrue")
	}
	return nil
}

//GetControlLatestUpgradeFlag Gets the day latest flag status from the pg database
func (u *UpgradesDB) GetControlLatestUpgradeFlagValue() (status bool, err error) {
	logrus.Info("Inside the get day latest flag")
	err = u.DB.QueryRow(getQueryForFlag(ControlIndexFlag)).Scan(&status)
	if err != nil {
		return status, errors.Wrapf(err, "PollForUpgradeFlagDayLatest unable to get current job status")
	}
	return status, err
}

//GetDayLatestUpgradeFlagValue Gets the day latest flag status from the pg database
func (u *UpgradesDB) GetDayLatestUpgradeFlagValue() (status bool, err error) {
	logrus.Info("Inside the get day latest flag")
	err = u.DB.QueryRow(getQueryForFlag(DayLatestFlag)).Scan(&status)
	if err != nil {
		return status, errors.Wrapf(err, "PollForUpgradeFlagDayLatest unable to get current job status")
	}
	return status, err
}

//getQueryForFlag gets the query for flag
func getQueryForFlag(flag string) string {
	return fmt.Sprintf("Select upgrade_value from upgrade_flags where upgrade_flag='%s'", flag)
}

//getUpdateQuery gets the update query for flag
func getUpdateQuery(flag string) string {
	return fmt.Sprintf("Update upgrade_flags set upgrade_value=false where upgrade_flag='%s'", flag)
}
