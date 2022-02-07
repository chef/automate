package majorupgradechecklist

import (
	"github.com/chef/automate/components/automate-cli/pkg/status"
	"github.com/chef/automate/components/automate-deployment/pkg/cli"
)

type ChecklistInspector interface {
	RunChecklist() error
	GetPostChecklists() ([]string, error)
}

//NewChecklistInspector returns the checklist inspector for given major release
func NewChecklistInspector(writer cli.FormatWriter, version, major string) (ChecklistInspector, error) {
	switch major {
	case "22":
		return NewV22ChecklistInspector(writer, version), nil
	default:
		return nil, status.Errorf(status.UpgradeError, "invalid major version")
	}
}

type Checklist struct {
	Name        string
	Description string
	TestFunc    func(ChecklistHelper) error
}

type ChecklistHelper struct {
	Writer cli.FormatWriter
}
