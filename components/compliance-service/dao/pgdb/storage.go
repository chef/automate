package pgdb

type Storage interface {
	GetDayLatestUpgradeFlagValue() (status bool, err error)
	GetControlLatestUpgradeFlagValue() (status bool, err error)
	GetUpgradeFlags() (map[string]bool, error)
	UpdateDayLatestFlagToFalse() error
	UpdateControlFlagToFalse() error
	UpdateCompRunInfoFlagToTrue() error
}

const DayLatestFlag = "day_latest"

const ControlIndexFlag = "control_index"

const CompRunInfoFlag = "comp_run_info"

type Flag struct {
	flag   string
	status bool
}
