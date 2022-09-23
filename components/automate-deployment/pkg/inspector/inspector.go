package inspector

type Inspector interface {
	AddInspection(inspection Inspection)
	ShowInfo() error
	ShowInspectionList()
	Inspect() error
	PreExit() error
	ShowExitMessages() error
}

type Inspection interface {
	ShowInfo(index *int) error
	HasExitedWithError() bool
	SetExitedWithError(status bool)
	PrintExitMessage() error
}

type SystemInspection interface {
	Inspection
	Inspect() error
	Skip()
	GetShortInfo() []string
	GetInstallationType() InstallationType
}

type ExitInspection interface {
	SystemInspection
	PreExit() error
	GetIsExecuted() bool
}

type InstallationType int

const (
	EMBEDDED InstallationType = iota
	EXTERNAL InstallationType = iota
	BOTH     InstallationType = iota
)
