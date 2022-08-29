package pgdb

type Storage interface {
	GetUpgradeFlags() (map[string]bool, error)
	UpdateControlFlagToFalse() error
}

const DayLatestFlag = "day_latest"

const ControlIndexFlag = "control_index"

const CompRunInfoFlag = "comp_run_info"

type Flag struct {
	flag   string
	status bool
}
