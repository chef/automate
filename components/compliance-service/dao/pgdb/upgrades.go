package pgdb

import (
	"fmt"
	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"
	"strings"
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

//UpdateCompRunInfoFlagToTrue updates the day latest flags to true
func (u *UpgradesDB) UpdateCompRunInfoFlagToTrue() error {
	_, err := u.DB.Exec(getUpdateQuery(CompRunInfoFlag))
	if err != nil {
		return errors.Wrapf(err, "Unable to UpdateDayLatestFlagToTrue")
	}
	return nil
}

//GetControlLatestUpgradeFlag Gets the day latest flag status from the pg database
func (u *UpgradesDB) GetControlLatestUpgradeFlagValue() (status bool, err error) {
	logrus.Info("Inside the get control index flag")
	return status, err
}

//GetDayLatestUpgradeFlagValue Gets the day latest flag status from the pg database
func (u *UpgradesDB) GetDayLatestUpgradeFlagValue() (status bool, err error) {
	logrus.Info("Inside the day latest flag")
	return status, err
}

//GetDayLatestUpgradeFlagValue Gets the day latest flag status from the pg database
func (u *UpgradesDB) GetUpgradeFlags() (map[string]bool, error) {
	flagMap := make(map[string]bool)

	logrus.Info("Inside the comp run info flag")
	flags := []string{CompRunInfoFlag, ControlIndexFlag, DayLatestFlag}
	rows, err := u.DB.Query(getQueryForFlag(flags))
	if err != nil {
		return flagMap, err
	}
	defer func() {
		if err := rows.Close(); err != nil {
			logrus.Errorf("failed to close db rows: %s", err.Error())
		}
	}()

	for rows.Next() {
		flag := Flag{}
		if err := rows.Scan(&flag.flag, &flag.status); err != nil {
			logrus.Errorf("Unable to get the flags with error %v", err)
			return nil, err
		}
		flagMap[flag.flag] = flag.status
	}
	if err := rows.Err(); err != nil {
		return nil, errors.Wrap(err, "error retrieving result rows")
	}
	return flagMap, err
}

//getQueryForFlag gets the query for flag
func getQueryForFlag(flag []string) string {
	flags := `'` + strings.Join(flag, `','`) + `'`
	return fmt.Sprintf("Select upgrade_flag,upgrade_value from upgrade_flags where upgrade_flag in (%s)", flags)
}

//getUpdateQuery gets the update query for flag
func getUpdateQuery(flag string) string {
	return fmt.Sprintf("Update upgrade_flags set upgrade_value=false where upgrade_flag='%s'", flag)
}
