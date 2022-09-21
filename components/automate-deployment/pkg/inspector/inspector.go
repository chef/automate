package inspector

type Inspector interface {
	ShowInfo() error
	Inspect() error
	AddInspection(inspection Inspection)
	ShowInspectionList()
}

type Inspection interface {
	ShowInfo(index *int) error
}

type SystemInspection interface {
	Inspection
	Inspect() error
	Skip()
	GetShortInfo() []string
}
