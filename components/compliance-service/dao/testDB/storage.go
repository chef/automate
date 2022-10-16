package testDB

import (
	"github.com/chef/automate/components/compliance-service/dao/pgdb"
	"github.com/pkg/errors"
	"time"
)

type UpgradeDBTest struct {
	Error                bool
	NeedStatus           bool
	NeedNotStartedStatus bool
	NeedInProgressStatus bool
	NeedCompletedStatus  bool
	NeedError            bool
}

func (u UpgradeDBTest) GetUpgradeFlags() (map[string]pgdb.Flag, error) {
	if u.Error {
		return nil, errors.New("Unable to fetch status from database")
	}

	flagsMap := make(map[string]pgdb.Flag)

	if u.NeedError {
		return nil, errors.New("There is an error")
	}
	if u.NeedStatus {
		flagsMap[pgdb.ControlIndexFlag] = pgdb.Flag{Status: true, UpgradedTime: time.Now()}
		flagsMap[pgdb.CompRunInfoFlag] = pgdb.Flag{Status: true, UpgradedTime: time.Now()}
		flagsMap[pgdb.DayLatestFlag] = pgdb.Flag{Status: true, UpgradedTime: time.Now()}
	}
	if u.NeedNotStartedStatus {
		flagsMap[pgdb.ControlIndexFlag] = pgdb.Flag{Status: true, UpgradedTime: time.Time{}}
	}
	if u.NeedInProgressStatus {
		flagsMap[pgdb.ControlIndexFlag] = pgdb.Flag{Status: true, UpgradedTime: time.Now()}
	}
	if u.NeedCompletedStatus {
		flagsMap[pgdb.ControlIndexFlag] = pgdb.Flag{Status: false, UpgradedTime: time.Now()}
	}

	return flagsMap, nil
}

func (u UpgradeDBTest) UpdateControlFlagToFalse() error {
	return nil
}

func (u UpgradeDBTest) UpdateControlFlagTimeStamp() error {
	return nil
}
