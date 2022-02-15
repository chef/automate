package majorupgradechecklist

import (
	"github.com/chef/automate/components/automate-cli/pkg/status"
	"github.com/chef/automate/components/automate-deployment/pkg/cli"
	"github.com/chef/automate/components/automate-deployment/pkg/manifest"
)

type ChecklistManager interface {
	RunChecklist() error
	CreatePostChecklistFile() error
	ReadPostChecklistFile() ([]string, error)
	ReadPostChecklistById(id string) (bool, error)
	UpdatePostChecklistFile(id string) error
}

type PerCheckList struct {
	Msg      string `json:"msg"`
	Response string `json:"-"`
}

type PostCheckList struct {
	Id         string `json:"id"`
	Msg        string `json:"msg"`
	Cmd        string `json:"cmd"`
	Optional   bool   `json:"optional"`
	IsExecuted bool   `json:"is_executor"`
}

type PerPostChecklist struct {
	PostChecklist []PostCheckList `json:"post_checklist"`
}

//NewChecklistManager returns the checklist inspector for given major release

func NewChecklistManager(writer cli.FormatWriter, version, major string) (ChecklistManager, error) {

	if major == "" {
		resp, _ := manifest.IsSemVersionFmt(version)
		major = resp
	}

	switch major {
	case "2":
		return NewV22ChecklistManager(writer, version), nil
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
