package majorupgradechecklist

import (
	"bytes"
	"encoding/json"
	"fmt"
	"io/ioutil"

	"github.com/chef/automate/components/automate-cli/pkg/status"
	"github.com/chef/automate/components/automate-deployment/pkg/cli"
	"github.com/chef/automate/components/automate-deployment/pkg/manifest"
	platform_config "github.com/chef/automate/lib/platform/config"
	"github.com/pkg/errors"
)

const (
	initMsg = `This is a Major upgrade. 
========================

  1) In this release Embedded PostgreSQL is upgraded to version 13.5 
  2) This will need special care if you use Embedded PostgreSQL. 

===== Your installation is using %s PostgreSQL =====

  Please confirm this checklist that you have taken care of these steps 
  before continuing with the Upgrade to version %s:
`

	downTimeError = "There will be a downtime while upgrading. Please prepare for down time and run the upgrade"

	backupError = "Please take a backup and restart the upgrade process."

	run_chef_automate_upgrade_status_cmd = `chef-automate upgrade status`
	run_chef_automate_upgrade_status     = `Check the status of your upgrade using:  
     $ ` + run_chef_automate_upgrade_status_cmd + `
   This should return: Automate is up-to-date`

	run_pg_data_migrate = `Migrate Data from PG 9.6 to PG 13.5 using this command:
     $ ` + run_pg_data_migrate_cmd

	run_pg_data_migrate_cmd = `chef-automate post-major-upgrade migrate --data=pg`

	run_chef_automate_status_cmd = `chef-automate status`
	run_chef_automate_status     = `Check all services are running using: 
     $ chef-automate status`

	run_pg_data_cleanup = `If you are sure all data is available in Upgraded Automate, then we can free up old PostgreSQL 9.6 Data by running: 
     $ ` + run_pg_data_cleanup_cmd

	run_pg_data_cleanup_cmd        = `chef-automate post-major-upgrade clear-data --pg-data`
	v3_post_checklist_confirmation = `**** In case of any errors, please refer to docs.chef.io and release notes for this version. ****

Now, upgrade will start, Please confirm to continue...`
	ui_check           = `Check Automate UI everything is running and all data is visible`
	patch_new_conf_cmd = `chef-automate config patch config.toml`
	patch_new_conf     = `If your PostgreSQL Connection URL and Credential are changed then update them by putting them in config.toml and patching it in using:
     $ chef-automate config patch config.toml`
	post_upgrade_header = `
Post Upgrade Steps:
===================
`
)

var postChecklistEmbedded = []PostCheckList{
	{
		Id:         "upgrade_status",
		Msg:        run_chef_automate_upgrade_status,
		Cmd:        run_chef_automate_upgrade_status_cmd,
		Optional:   true,
		IsExecuted: false,
	}, {
		Id:         "migrate_pg",
		Msg:        run_pg_data_migrate,
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
		Msg:        run_pg_data_cleanup,
		Cmd:        run_pg_data_cleanup_cmd,
		Optional:   true,
		IsExecuted: false,
	},
}

var postChecklistExternal = []PostCheckList{
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

type V3ChecklistManager struct {
	writer  cli.FormatWriter
	version string
}

func NewV3ChecklistManager(writer cli.FormatWriter, version string) *V3ChecklistManager {
	return &V3ChecklistManager{
		writer:  writer,
		version: version,
	}
}

type checkFunc func() Checklist

func preChecklist(check checkFunc) []Checklist {
	return []Checklist{
		downTimeCheck(),
		backupCheck(),
		check(),
		postChecklistIntimationCheck(),
	}
}

func isExternalPG() bool {
	config, err := platform_config.ConfigFromParams("pg-sidecar-service", "/hab/svc/pg-sidecar-service", "")
	if err != nil {
		fmt.Println("error in config from environment")
	}
	return config.IsExternalPG()
}

func (ci *V3ChecklistManager) RunChecklist() error {

	var dbType string
	checklists := []Checklist{}
	var postcheck []PostCheckList

	if isExternalPG() {
		dbType = "External"
		postcheck = postChecklistExternal
		checklists = append(checklists, preChecklist(externalPGUpgradeCheck)...)
	} else {
		dbType = "Embedded"
		postcheck = postChecklistEmbedded
		checklists = append(checklists, preChecklist(diskSpaceCheck)...)
	}
	checklists = append(checklists, ci.showPostChecklist(postcheck), promptUpgradeContinue())

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

func (ci *V3ChecklistManager) showPostChecklist(postCheck []PostCheckList) Checklist {
	return Checklist{
		Name:        "Show_Post_Checklist",
		Description: "Show Post Checklist",
		TestFunc: func(h ChecklistHelper) error {
			displayed := false
			for i, item := range postCheck {
				if !item.IsExecuted {
					if !displayed {
						h.Writer.Println(post_upgrade_header)
						displayed = true
					}
					h.Writer.Println(fmt.Sprintf("%d", i+1) + ") " + item.Msg)
					h.Writer.Println("")
				}
			}
			return nil
		},
	}

}

func downTimeCheck() Checklist {
	return Checklist{
		Name:        "down_time_acceptance",
		Description: "confirmation for downtime",
		TestFunc: func(h ChecklistHelper) error {
			resp, err := h.Writer.Confirm("You had planned for a downtime?:")
			if err != nil {
				h.Writer.Error(err.Error())
				return status.Errorf(status.InvalidCommandArgsError, err.Error())
			}
			if !resp {
				h.Writer.Error(downTimeError)
				return status.New(status.InvalidCommandArgsError, downTimeError)
			}
			return nil
		},
	}
}

func backupCheck() Checklist {
	return Checklist{
		Name:        "backup_acceptance",
		Description: "confirmation check for creating a backup",
		TestFunc: func(h ChecklistHelper) error {
			resp, err := h.Writer.Confirm("You have taken backup of your data and kept it safe, preferred on other disk or location?")
			if err != nil {
				h.Writer.Error(err.Error())
				return status.Errorf(status.InvalidCommandArgsError, err.Error())
			}
			if !resp {
				h.Writer.Error(backupError)
				return status.New(status.InvalidCommandArgsError, downTimeError)
			}
			return nil
		},
	}
}

func diskSpaceCheck() Checklist {
	return Checklist{
		Name:        "disk_space_acceptance",
		Description: "confirmation check for disk space",
		TestFunc: func(h ChecklistHelper) error {
			resp, err := h.Writer.Confirm("Ensure you have more than 60 percent free disk space")
			if err != nil {
				h.Writer.Error(err.Error())
				return status.Errorf(status.InvalidCommandArgsError, err.Error())
			}
			if !resp {
				h.Writer.Error(backupError)
				return status.New(status.InvalidCommandArgsError, downTimeError)
			}
			return nil
		},
	}
}

func postChecklistIntimationCheck() Checklist {
	return Checklist{
		Name:        "post_checklist_intimation_acceptance",
		Description: "confirmation check for post checklist intimation",
		TestFunc: func(h ChecklistHelper) error {
			resp, err := h.Writer.Confirm("After this upgrade completes, you will have to run Post upgrade steps to ensure your data is migrated and your Automate is ready for use")
			if err != nil {
				h.Writer.Error(err.Error())
				return status.Errorf(status.InvalidCommandArgsError, err.Error())
			}
			if !resp {
				h.Writer.Error(backupError)
				return status.New(status.InvalidCommandArgsError, downTimeError)
			}
			return nil
		},
	}
}

func externalPGUpgradeCheck() Checklist {
	return Checklist{
		Name:        "external_pg_upgrade_acceptance",
		Description: "confirmation check for external PG upgrade",
		TestFunc: func(h ChecklistHelper) error {
			resp, err := h.Writer.Confirm("Upgrade your PostgreSQL 9.6 to 13.5 with the help of your Database Administrator")
			if err != nil {
				h.Writer.Error(err.Error())
				return status.Errorf(status.InvalidCommandArgsError, err.Error())
			}
			if !resp {
				h.Writer.Error(backupError)
				return status.New(status.InvalidCommandArgsError, downTimeError)
			}
			return nil
		},
	}
}

func promptUpgradeContinue() Checklist {
	return Checklist{
		Name:        "post_checklist",
		Description: "display post checklist and ask for final confirmation",
		TestFunc: func(h ChecklistHelper) error {
			resp, err := h.Writer.Confirm(v3_post_checklist_confirmation)
			if err != nil {
				h.Writer.Error(err.Error())
				return status.Errorf(status.InvalidCommandArgsError, err.Error())
			}
			if !resp {
				h.Writer.Error("end user not ready to upgrade")
				return status.New(status.InvalidCommandArgsError, "end user not ready to upgrade")
			}
			return nil
		},
	}
}

func (ci *V3ChecklistManager) CreatePostChecklistFile() error {
	params := PerPostChecklist{}
	if isExternalPG() {
		params.PostChecklist = append(params.PostChecklist, postChecklistExternal...)
	} else {
		params.PostChecklist = append(params.PostChecklist, postChecklistEmbedded...)
	}
	version, is_major_version := manifest.IsSemVersionFmt(ci.version)
	if is_major_version {
		params.Version = version
	}

	var buffer bytes.Buffer
	data, err := json.Marshal(params)
	if err != nil {
		return err
	}
	buffer.Write(data)
	buffer.WriteString("\n")
	err = ioutil.WriteFile("/hab/svc/deployment-service/var/upgrade_metadata.json", buffer.Bytes(), 0644)
	if err != nil {
		return err
	}
	return nil
}
func (ci *V3ChecklistManager) ReadPostChecklistById(id string) (bool, error) {
	ChecklistId_Found := false
	res, err := ReadJsonFile()
	if err != nil {
		return false, err
	}
	for i := 0; i < len(res.PostChecklist); i++ {

		if res.PostChecklist[i].Id == id {
			ChecklistId_Found = res.PostChecklist[i].IsExecuted
			break

		}
	}
	return ChecklistId_Found, nil
}

func ReadJsonFile() (*PerPostChecklist, error) {
	byteValue, err := ioutil.ReadFile("/hab/svc/deployment-service/var/upgrade_metadata.json")
	if err != nil {
		return nil, err
	}
	params := PerPostChecklist{}

	err = json.Unmarshal(byteValue, &params)
	if err != nil {
		return nil, err
	}
	return &params, nil
}

func (ci *V3ChecklistManager) ReadPostChecklistFile() ([]string, error) {
	var postCmdList []string
	var showPostChecklist = false
	res, err := ReadJsonFile()
	if err != nil {
		return nil, err
	}

	if ci.version == res.Version {
		for i := 0; i < len(res.PostChecklist); i++ {
			if !res.PostChecklist[i].Optional && !res.PostChecklist[i].IsExecuted {
				showPostChecklist = true
				break
			}
		}

		if showPostChecklist == true {
			for i := 0; i < len(res.PostChecklist); i++ {
				if !res.PostChecklist[i].IsExecuted {
					postCmdList = append(postCmdList, res.PostChecklist[i].Msg)
				}
			}
		}
		return postCmdList, nil
	}

	return nil, errors.Errorf("Failed to read checklist since version didn't match")

}

func (ci *V3ChecklistManager) UpdatePostChecklistFile(id string) error {
	res, err := ReadJsonFile()
	if err != nil {
		return err
	}
	for i, v := range res.PostChecklist {
		if v.Id == id {
			res.PostChecklist[i].IsExecuted = true
		}
	}

	data, err := json.Marshal(res)
	if err != nil {
		return err
	}
	err = ioutil.WriteFile("/hab/svc/deployment-service/var/upgrade_metadata.json", data, 0644)
	if err != nil {
		return err
	}
	return nil
}
