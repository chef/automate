package majorupgradechecklist

import (
	"github.com/chef/automate/components/automate-cli/pkg/status"
	"github.com/chef/automate/components/automate-deployment/pkg/cli"
)

type ChecklistManager interface {
	RunChecklist() error
	GetPostChecklist() []PostCheckListItem
}

func NewChecklistManager(writer cli.FormatWriter, version, major string) (ChecklistManager, error) {

	if major == "" {
		major, _ = GetMajorVersion(version)
	}

	switch major {
	case "3":
		return NewV3ChecklistManager(writer, version), nil
	default:
		return nil, status.Errorf(status.UpgradeError, "invalid major version")
	}
}
