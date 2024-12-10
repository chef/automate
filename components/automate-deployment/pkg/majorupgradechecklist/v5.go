package majorupgradechecklist

import (
	"fmt"

	"github.com/chef/automate/components/automate-deployment/pkg/cli"
	"github.com/pkg/errors"
)

type V5ChecklistManager struct {
	writer       cli.FormatWriter
	version      string
	isExternalPG bool
}

func NewV5ChecklistManager(writer cli.FormatWriter, version string) *V5ChecklistManager {
	return &V5ChecklistManager{
		writer:       writer,
		version:      version,
		isExternalPG: IsExternalPG(),
	}
}

func (ci *V5ChecklistManager) GetPostChecklist() []PostCheckListItem {
	var postChecklist []PostCheckListItem
	if ci.isExternalPG {
		postChecklist = postChecklistExternal
	} else {
		postChecklist = postChecklistEmbedded
	}
	return postChecklist
}

func (ci *V5ChecklistManager) RunChecklist(timeout int64, flags ChecklistUpgradeFlags) error {

	var dbType string
	checklists := []Checklist{}
	var postcheck []PostCheckListItem

	if ci.isExternalPG {
		dbType = "External"
		postcheck = postChecklistExternal
		checklists = append(checklists, []Checklist{downTimeCheck(), backupCheck(), externalPGUpgradeCheck(), postChecklistIntimationCheck()}...)
	} else {
		dbType = "Embedded"
		postcheck = postChecklistEmbedded
		checklists = append(checklists, []Checklist{diskSpaceCheck(ci.version, flags.SkipStorageCheck, flags.OsDestDataDir), downTimeCheck(), backupCheck(), postChecklistIntimationCheck()}...)
	}
	checklists = append(checklists, showPostChecklist(&postcheck), promptUpgradeContinue())

	helper := ChecklistHelper{
		Writer: ci.writer,
	}

	ci.writer.Println(fmt.Sprintf(initMsg, dbType, ci.version)) //display the init message

	for _, item := range checklists {
		if item.TestFunc == nil {
			continue
		}
		if err := item.TestFunc(helper); err != nil {
			return errors.Wrap(err, "one of the checklist was not accepted/satisfied for upgrade")
		}
	}
	return nil
}
