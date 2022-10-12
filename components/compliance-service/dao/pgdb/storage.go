package pgdb

import "time"

type Storage interface {
	GetUpgradeFlags() (map[string]Flag, error)
	UpdateControlFlagToFalse() error
	UpdateControlFlagTimeStamp() error
}

const DayLatestFlag = "day_latest"

const ControlIndexFlag = "control_index"

const CompRunInfoFlag = "comp_run_info"

type Flag struct {
	FlagName     string    `db:"upgrade_flag"`
	Status       bool      `db:"upgrade_value"`
	UpgradedTime time.Time `db:"upgrade_time"`
}
