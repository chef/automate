package pgdb

import "time"

type Storage interface {
	GetUpgradeFlags() (map[string]Flag, error)
	UpdateControlFlagValue(bool) error
	UpdateControlFlagTimeStamp() error
	AddEnhancedReportingFlag() error
	RemoveEnhancedReportingFlag() error
}

// constants represents the flags
const (
	DayLatestFlag                = "day_latest"
	ControlIndexFlag             = "control_index"
	CompRunInfoFlag              = "comp_run_info"
	EnhancedReportingEnabledFlag = "enhanced_reporting"
)

type Flag struct {
	FlagName     string    `db:"upgrade_flag"`
	Status       bool      `db:"upgrade_value"`
	UpgradedTime time.Time `db:"upgrade_time"`
}
