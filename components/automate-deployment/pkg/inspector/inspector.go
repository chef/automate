package inspector

type Inspector interface {
	ShowInfo() error
	Inspect() error
	AddInspection(inspection Inspection)
	ShowInspectionList()
	PreExit() error
}

type Inspection interface {
	ShowInfo(index *int) error
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
}

type InstallationType int

const (
	EMBEDDED InstallationType = iota
	EXTERNAL InstallationType = iota
	BOTH     InstallationType = iota
)
