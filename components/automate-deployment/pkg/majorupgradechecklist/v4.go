package majorupgradechecklist

import (
	"fmt"
	"io/ioutil"
	"net/http"
	"strings"

	"github.com/chef/automate/components/automate-cli/pkg/status"
	"github.com/chef/automate/components/automate-deployment/pkg/cli"
	"github.com/pkg/errors"
)

const (
	initMsgV4 = `This is a Major upgrade. 
========================

  1) In this release Embedded Elastic Search is migrated to Open Search
  2) This will need special care if you use Embedded Elastic Search. 

===== Your installation is using %s Elastic Search =====

  Please confirm this checklist that you have taken care of these steps 
  before continuing with the Upgrade to version %s:
`

	maintenanceModeError = "Please move Chef automate to maintenance mode before upgrading"

	externalESUpgradeError = "setup external open search and apply the configuration"

	run_os_data_migrate = `Migrate Data from Elastic Search to Open Search using this command:
     $ ` + run_os_data_migrate_cmd

	run_os_data_migrate_cmd = `chef-automate post-major-upgrade migrate --data=es`

	run_os_data_cleanup = `If you are sure all data is available in Upgraded Automate, then we can free up old elastic search Data by running: 
     $ ` + run_os_data_cleanup_cmd

	run_os_data_cleanup_cmd = `chef-automate post-major-upgrade clear-data --data=es`
)

var postChecklistV4Embedded = []PostCheckListItem{
	{
		Id:         "upgrade_status",
		Msg:        run_chef_automate_upgrade_status,
		Cmd:        run_chef_automate_upgrade_status_cmd,
		Optional:   true,
		IsExecuted: false,
	}, {
		Id:         "migrate_os",
		Msg:        run_os_data_migrate,
		Cmd:        run_os_data_migrate_cmd,
		IsExecuted: false,
	}, {
		Id:         "check_ui",
		Msg:        ui_check,
		Cmd:        "",
		Optional:   true,
		IsExecuted: false,
	}, {
		Id:         "clean_up",
		Msg:        run_os_data_cleanup,
		Cmd:        run_os_data_cleanup_cmd,
		Optional:   true,
		IsExecuted: false,
	},
}

var postChecklistV4External = []PostCheckListItem{
	{
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

type V4ChecklistManager struct {
	writer       cli.FormatWriter
	version      string
	isExternalES bool
}

func NewV4ChecklistManager(writer cli.FormatWriter, version string) *V4ChecklistManager {
	return &V4ChecklistManager{
		writer:       writer,
		version:      version,
		isExternalES: IsExternalElasticSearch(writer),
	}
}

func preChecklistV4(check checkFunc) []Checklist {
	return []Checklist{
		downTimeCheck(),
		backupCheck(),
		maintenanceModeCheck(),
		check(),
		disableShrding(),
		postChecklistIntimationCheck(),
	}
}

func (ci *V4ChecklistManager) GetPostChecklist(isExternalPG bool) []PostCheckListItem {
	var postChecklist []PostCheckListItem
	if ci.isExternalES {
		postChecklist = postChecklistV4External
	} else {
		postChecklist = postChecklistV4Embedded
	}
	return postChecklist
}

func (ci *V4ChecklistManager) RunChecklist(isExternalPG bool) error {
	var dbType string
	checklists := []Checklist{}
	var postcheck []PostCheckListItem

	if ci.isExternalES {
		dbType = "External"
		postcheck = postChecklistV4External
		checklists = append(checklists, preChecklistV4(externalESUpgradeCheck)...)
	} else {
		dbType = "Embedded"
		postcheck = postChecklistV4Embedded
		checklists = append(checklists, preChecklistV4(diskSpaceCheck)...)
	}
	checklists = append(checklists, showPostChecklist(&postcheck), promptUpgradeContinue())

	helper := ChecklistHelper{
		Writer: ci.writer,
	}

	ci.writer.Println(fmt.Sprintf(initMsgV4, dbType, ci.version)) //display the init message

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

func maintenanceModeCheck() Checklist {
	return Checklist{
		Name:        "maintenance_mode_acceptance",
		Description: "put automate into maintenance mode",
		TestFunc: func(h ChecklistHelper) error {
			resp, err := h.Writer.Confirm("Have you moved Chef automate to maintenance mode?:")
			if err != nil {
				h.Writer.Error(err.Error())
				return status.Errorf(status.InvalidCommandArgsError, err.Error())
			}
			if !resp {
				h.Writer.Error(downTimeError)
				return status.New(status.InvalidCommandArgsError, maintenanceModeError)
			}
			return nil
		},
	}
}

func externalESUpgradeCheck() Checklist {
	return Checklist{
		Name:        "external_es_upgrade_acceptance",
		Description: "confirmation check for external es upgrade",
		TestFunc: func(h ChecklistHelper) error {
			resp, err := h.Writer.Confirm("Upgrade your elastic search to Open Search with the help of your Administrator, and apply the configuration")
			if err != nil {
				h.Writer.Error(err.Error())
				return status.Errorf(status.InvalidCommandArgsError, err.Error())
			}
			if !resp {
				h.Writer.Error(backupError)
				return status.New(status.InvalidCommandArgsError, externalESUpgradeError)
			}
			return nil
		},
	}
}

/*
const disableShardCurlRequest = `curl --location --request PUT 'http://localhost:10141/_cluster/settings'
-u 'admin:admin' --header 'Content-Type: application/json'
--data-raw '{ "persistent": { "cluster.routing.allocation.enable": "primaries"}}'`

const flushIndexRequest = `curl --location --request PUT 'http://localhost:10168/_cluster/settings' \
-u 'admin:admin' \
--header 'Content-Type: application/json' \
--data-raw '{
  "persistent": {
    "cluster.routing.allocation.enable": "all"
  }}'`

*/
/*
disableShardAllocation : Disable shard allocation: during upgrade
we do not want any shard movement as the cluster will be
taken down.
*/
func disableShardAllocation() error {
	url := "http://localhost:10141/_cluster/settings"
	method := "PUT"

	payload := strings.NewReader(`{
    "persistent": {
        "cluster.routing.allocation.enable": "primaries"
    }
}`)

	client := &http.Client{}
	req, err := http.NewRequest(method, url, payload)

	if err != nil {
		return err
	}
	req.Header.Add("Content-Type", "application/json")

	res, err := client.Do(req)
	if err != nil {
		fmt.Println(err)
		return err
	}
	defer res.Body.Close()

	body, err := ioutil.ReadAll(res.Body)
	if err != nil {
		return err
	}
	fmt.Println(string(body))
	return nil
}

/*
flushRequest : Stop indexing, and perform a flush:
as the cluster will be taken down, indexing/searching
should be stopped and _flush can be used to permanently
store information into the index which will prevent any
data loss during upgrade
*/
func flushRequest() error {

	url := "http://localhost:10141/_flush"
	method := "POST"

	client := &http.Client{}
	req, err := http.NewRequest(method, url, nil)

	if err != nil {
		fmt.Println(err)
		return err
	}
	req.Header.Add("Content-Type", "application/json")

	res, err := client.Do(req)
	if err != nil {
		return err
	}
	defer res.Body.Close()

	body, err := ioutil.ReadAll(res.Body)
	if err != nil {
		fmt.Println(err)
		return err
	}
	fmt.Println(string(body))
	return nil
}

/*
reEnableShardAllocation: Re-enable shard allocation
In case of Any error we can Undo the First request
*/
func reEnableShardAllocation() error {
	url := "http://localhost:10141/_cluster/settings"
	method := "PUT"

	payload := strings.NewReader(`{
		"persistent": {
			"cluster.routing.allocation.enable": null
		  }
}`)

	client := &http.Client{}
	req, err := http.NewRequest(method, url, payload)

	if err != nil {
		return err
	}
	req.Header.Add("Content-Type", "application/json")

	res, err := client.Do(req)
	if err != nil {
		fmt.Println(err)
		return err
	}
	defer res.Body.Close()

	body, err := ioutil.ReadAll(res.Body)
	if err != nil {
		return err
	}
	fmt.Println(string(body))
	return nil
}
func disableShrding() Checklist {
	return Checklist{
		Name:        "disable sharding",
		Description: "confirmation check to disable sharding",
		TestFunc: func(h ChecklistHelper) error {
			resp, err := h.Writer.Confirm("Sharding will be disbled")
			if err != nil {
				h.Writer.Error(err.Error())
				return status.Errorf(status.InvalidCommandArgsError, err.Error())
			}
			if !resp {
				h.Writer.Error(backupError)
				return status.New(status.InvalidCommandArgsError, "sharing has to be disabled before upgrade")
			}
			err = disableShardAllocation()
			if err != nil {
				h.Writer.Error(err.Error())
				return status.Errorf(status.DatabaseError, err.Error())
			}

			err = flushRequest()
			if err != nil {
				h.Writer.Error(err.Error())
				return status.Errorf(status.DatabaseError, err.Error())
			}
			return nil
		},
	}
}
