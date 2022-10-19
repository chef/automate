package migrations

import (
	"github.com/chef/automate/components/compliance-service/dao/pgdb"
	"github.com/pkg/errors"
)

type UpgradeDBTest struct {
	Error      bool
	NeedStatus bool
}

func (u UpgradeDBTest) GetUpgradeFlags() (map[string]pgdb.Flag, error) {
	if u.Error {
		return nil, errors.New("Unable to fetch status from database")
	}

	flagsMap := make(map[string]pgdb.Flag)

	if u.NeedStatus {
		flagsMap[pgdb.ControlIndexFlag] = pgdb.Flag{FlagName: pgdb.ControlIndexFlag, Status: true}
		flagsMap[pgdb.CompRunInfoFlag] = pgdb.Flag{FlagName: pgdb.CompRunInfoFlag, Status: true}
		flagsMap[pgdb.DayLatestFlag] = pgdb.Flag{FlagName: pgdb.DayLatestFlag, Status: true}
		return flagsMap, nil
	}
	return flagsMap, nil
}

func (u UpgradeDBTest) UpdateControlFlagValue(b bool) error {
	return nil
}

func (u UpgradeDBTest) RemoveEnhancedReportingFlag() error {
	return nil
}

func (u UpgradeDBTest) UpdateControlFlagTimeStamp() error {
	return nil
}

func (u UpgradeDBTest) AddEnhancedReportingFlag() error {
	return nil
}
