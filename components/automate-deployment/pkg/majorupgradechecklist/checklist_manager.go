package majorupgradechecklist

import (
	"github.com/chef/automate/components/automate-cli/pkg/status"
	"github.com/chef/automate/components/automate-deployment/pkg/cli"
)

type ChecklistUpgradeFlags struct {
	SkipStorageCheck bool
	OsDestDataDir    string
}

type ChecklistManager interface {
	RunChecklist(int64, ChecklistUpgradeFlags) error
	GetPostChecklist() []PostCheckListItem
}

func NewChecklistManager(writer cli.FormatWriter, version string) (ChecklistManager, error) {

	major, _ := GetMajorVersion(version)

	switch major {
	case "3":
		return NewV3ChecklistManager(writer, version), nil
	case "4":
		return NewV4ChecklistManager(writer, version), nil
	case "5":
		return NewV5ChecklistManager(writer, version), nil
	default:
		return nil, status.Errorf(status.UpgradeError, "invalid major version")
	}
}
