package migrations

import (
	"fmt"
	"github.com/chef/automate/components/compliance-service/dao/pgdb"
	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"
)

type UpgradesDB struct {
	DB *pgdb.DB
}

func NewDB(db *pgdb.DB) *UpgradesDB {
	return &UpgradesDB{db}
}

const dayLatestFlag = "day_latest"

//UpdateDayLatestFlagToTrue updates the day latest flags to true
func (u *UpgradesDB) UpdateDayLatestFlagToTrue() error {
	_, err := u.DB.Exec(getUpdateQuery(dayLatestFlag))
	if err != nil {
		return errors.Wrapf(err, "Unable to UpdateDayLatestFlagToTrue")
	}
	return nil
}

//GetDayLatestUpgradeFlagValue Gets the day latest flag status from the pg database
func (u *UpgradesDB) GetDayLatestUpgradeFlagValue() (status bool, err error) {
	logrus.Info("Inside the get day latest flag")
	err = u.DB.QueryRow(getQueryForFlag(dayLatestFlag)).Scan(&status)
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
	return fmt.Sprintf("Update upgrade_flags set upgrade_value=true where upgrade_flag='%s'", flag)
}
