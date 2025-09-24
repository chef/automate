package majorupgradechecklist

import (
	"fmt"

	"github.com/chef/automate/components/automate-deployment/pkg/cli"
	"github.com/pkg/errors"
)

var postChecklistEmbeddedV5 = []PostCheckListItem{
	{
		Id:         "upgrade_status",
		Msg:        run_chef_automate_upgrade_status,
		Cmd:        run_chef_automate_upgrade_status_cmd,
		Optional:   true,
		IsExecuted: false,
	}, {
		Id:         "migrate_pg",
		Msg:        fmt.Sprintf(run_pg_data_migrate, 13.5, 17.0),
		Cmd:        run_pg_data_migrate_cmd,
		IsExecuted: false,
	}, {
		Id:         "check_ui",
		Msg:        ui_check,
		Cmd:        "",
		Optional:   true,
		IsExecuted: false,
	}, {
		Id:         "clean_up",
		Msg:        fmt.Sprintf(run_pg_data_cleanup, 13.5),
		Cmd:        run_pg_data_cleanup_cmd,
		Optional:   true,
		IsExecuted: false,
	},
}

var postChecklistExternalV5 = []PostCheckListItem{
	{
		Id:         "patch_new_config",
		Msg:        patch_new_conf,
		Cmd:        patch_new_conf_cmd,
		Optional:   true,
		IsExecuted: false,
	}, {
		Id:         "upgrade_status",
		Msg:        run_chef_automate_upgrade_status,
		Cmd:        run_chef_automate_upgrade_status_cmd,
		Optional:   true,
		IsExecuted: false,
	}, {
		Id:         "status",
		Msg:        run_chef_automate_status,
		Cmd:        run_chef_automate_status_cmd,
		Optional:   true,
		IsExecuted: false,
	}, {
		Id:         "check_ui",
		Msg:        ui_check,
		Cmd:        "",
		Optional:   true,
		IsExecuted: false,
	},
}

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
		postChecklist = postChecklistExternalV5
	} else {
		postChecklist = postChecklistEmbeddedV5
	}
	return postChecklist
}

func (ci *V5ChecklistManager) RunChecklist(timeout int64, flags ChecklistUpgradeFlags) error {

	var dbType string
	checklists := []Checklist{}
	var postcheck []PostCheckListItem

	if ci.isExternalPG {
		dbType = "External"
		postcheck = postChecklistExternalV5
		checklists = append(checklists, []Checklist{downTimeCheck(), backupCheck(), externalPGUpgradeCheck(13.5, 17.0), postChecklistIntimationCheck()}...)
	} else {
		dbType = "Embedded"
		postcheck = postChecklistEmbeddedV5
		checklists = append(checklists, []Checklist{diskSpaceCheck(ci.version, flags.SkipStorageCheck, flags.OsDestDataDir), downTimeCheck(), backupCheck(), postChecklistIntimationCheck()}...)
	}
	checklists = append(checklists, showPostChecklist(&postcheck), promptUpgradeContinue())

	helper := ChecklistHelper{
		Writer: ci.writer,
	}

	ci.writer.Println(fmt.Sprintf(initMsg, 17.0, dbType, ci.version)) //display the init message

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
