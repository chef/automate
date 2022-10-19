package pgdb

import (
	"fmt"
	"strings"
	"time"

	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"
)

type UpgradesDB struct {
	DB *DB
}

func NewDB(db *DB) *UpgradesDB {
	return &UpgradesDB{db}
}

//UpdateControlFlagValue updates the upgrade_value for control index flag
func (u *UpgradesDB) UpdateControlFlagValue(value bool) error {
	_, err := u.DB.Exec(getUpdateQueryForValue(), value, ControlIndexFlag)
	if err != nil {
		return errors.Wrapf(err, "Unable to set Control Index upgrade_value to %t", value)
	}
	return nil
}

//GetUpgradeFlags Gets the all the upgrade flags and status from the pg database
func (u *UpgradesDB) GetUpgradeFlags() (map[string]Flag, error) {
	flagMap := make(map[string]Flag)

	logrus.Info("Inside the comp run info flag")
	flags := []string{ControlIndexFlag, EnhancedReportingEnabledFlag}
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
		if err := rows.Scan(&flag.FlagName, &flag.Status, &flag.UpgradedTime); err != nil {
			logrus.Errorf("Unable to get the flags with error %v", err)
			return nil, err
		}
		flagMap[flag.FlagName] = flag
	}
	if err := rows.Err(); err != nil {
		return nil, errors.Wrap(err, "error retrieving result rows")
	}
	return flagMap, err
}

// UpdateControlFlagTimeStamp updates the upgrade_time for the control index flag
func (u *UpgradesDB) UpdateControlFlagTimeStamp() error {
	_, err := u.DB.Exec(getUpdateQueryForTime(ControlIndexFlag), time.Now(), ControlIndexFlag)
	if err != nil {
		err = errors.Wrapf(err, "Unable to update the upgrade_time of upgrade_flags for control_index flag")
	}
	return err
}

// AddEnhancedReportingFlag adds the enhanced_reporting flag to flags table
func (u *UpgradesDB) AddEnhancedReportingFlag() error {
	_, err := u.DB.Exec(insertQuery(), 3, EnhancedReportingEnabledFlag, false, time.Now())
	if err != nil {
		err = errors.Wrapf(err, "Unable to add the enhanced_reporting flag to upgrade_flags table")
	}
	return err
}

// RemoveEnhancedReportingFlag delete the enhanced_reporting from flags table
func (u *UpgradesDB) RemoveEnhancedReportingFlag() error {
	_, err := u.DB.Exec(deleteFlag(), EnhancedReportingEnabledFlag)
	if err != nil {
		err = errors.Wrapf(err, "Unable to remove the enhanced_reporting flag from upgrade_flags")
	}
	return err
}

//getQueryForFlag gets the query for flag
func getQueryForFlag(flag []string) string {
	flags := `'` + strings.Join(flag, `','`) + `'`
	return fmt.Sprintf("Select upgrade_flag,upgrade_value, upgrade_time from upgrade_flags where upgrade_flag in (%s)", flags)
}

//getUpdateQueryForValue gets the update query for setting the upgrade_value
func getUpdateQueryForValue() string {
	return fmt.Sprintf("Update upgrade_flags set upgrade_value= $1 where upgrade_flag= $2")
}

func getUpdateQueryForTime(flag string) string {
	return fmt.Sprintf("Update upgrade_flags set upgrade_time= $1 where upgrade_flag= $2")
}

func insertQuery() string {
	return fmt.Sprintf("INSERT INTO UPGRADE_FLAGS (id,upgrade_flag,upgrade_value,upgrade_time) VALUES ($1,$2,$3,$4)")
}

func deleteFlag() string {
	return fmt.Sprintf("delete from upgrade_flags where upgrade_flag = $1")
}
