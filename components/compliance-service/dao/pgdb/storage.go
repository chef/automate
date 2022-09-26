package pgdb

type Storage interface {
	GetDayLatestUpgradeFlagValue() (status bool, err error)
	GetControlLatestUpgradeFlagValue() (status bool, err error)
	UpdateDayLatestFlagToFalse() error
	UpdateControlFlagToFalse() error
}

const DayLatestFlag = "day_latest"

const ControlIndexFlag = "control_index"
