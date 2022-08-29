package migrations

import (
	"github.com/chef/automate/components/compliance-service/dao/pgdb"
	"github.com/pkg/errors"
)

type UpgradeDBTest struct {
	Error      bool
	NeedStatus bool
}

func (u UpgradeDBTest) GetUpgradeFlags() (map[string]bool, error) {
	if u.Error {
		return nil, errors.New("Unable to fetch status from database")
	}

	flagsMap := make(map[string]bool)

	if u.NeedStatus {
		flagsMap[pgdb.ControlIndexFlag] = true
		flagsMap[pgdb.CompRunInfoFlag] = true
		flagsMap[pgdb.DayLatestFlag] = true
		return flagsMap, nil
	}
	return flagsMap, nil
}

func (u UpgradeDBTest) UpdateControlFlagToFalse() error {
	return nil
}
