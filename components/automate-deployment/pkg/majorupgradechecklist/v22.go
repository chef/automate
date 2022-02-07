package majorupgradechecklist

import (
	"fmt"

	"github.com/chef/automate/components/automate-cli/pkg/status"
	"github.com/chef/automate/components/automate-deployment/pkg/cli"
	"github.com/pkg/errors"
)

const (
	initMsg = `This is a Major upgrade. 
	In this release Embedded PostgreSQL is upgraded to version 13.5 
	Which will need special care if you use Embedded PostgreSQL. 
	*Your installation is using Embedded PostgreSQL.* 
	Please confirm this check list that you have completed these steps before continuing with the Upgrade to version %s:`

	downTimeError = "There will be a downtime while upgrading. Please preare for down time and run the upgrade"

	backupError = "Please take a backup and restart the upgrade process."

	run_chef_automate_upgrade_status = `Check the status of your upgrade using:  
		   $ chef-automate upgrade status 
	
		 This should return: Automate is up-to-date`

	run_pg_data_migrate = `Migrate Data from PG 9.6 to PG 13.5 using this command: 
		   $ chef-automate post-major-upgrade migrate --pg-data`

	run_chef_automate_status = `Check all services are running using: 
		   $ chef-automate status`

	check_ui = `Check Automate UI everything is running and all data is visible.`

	run_pg_data_cleanup = `If you are sure all data is available in Upgraded Automate, then we can free up old PostgreSQL 9.6 Data by running: 
		   $ chef-automate post-major-upgrade clear-data --pg-data`

	v22_post_checklist_confirmation = `*In case of any errors, please refer to docs.chef.io and release notes for this version.*
	Now, upgrade will start, Please confirm to continue...`
)

type V22ChecklistInspector struct {
	writer  cli.FormatWriter
	version string
}

func NewV22ChecklistInspector(writer cli.FormatWriter, version string) *V22ChecklistInspector {
	return &V22ChecklistInspector{
		writer:  writer,
		version: version,
	}
}

func (ci *V22ChecklistInspector) RunChecklist() error {
	checklists := []Checklist{
		downTimeCheck(),
		backupCheck(),
		v22PostChecklist(),
	}

	helper := ChecklistHelper{
		Writer: ci.writer,
	}

	ci.writer.Println(fmt.Sprintf(initMsg, ci.version)) //display the init message

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

func (ci *V22ChecklistInspector) GetPostChecklists() ([]string, error) {
	return getPostChecklist(), nil
}

func getPostChecklist() []string {
	return []string{run_chef_automate_upgrade_status, run_pg_data_migrate, run_chef_automate_status, check_ui, run_pg_data_cleanup}
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
			resp, err := h.Writer.Confirm("You have taken backup of your data and is safe, preferred on other disk or location?")
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

func v22PostChecklist() Checklist {
	return Checklist{
		Name:        "post_checklist",
		Description: "display post checklist and ask for final confirmation",
		TestFunc: func(h ChecklistHelper) error {
			h.Writer.Title("Post Upgrade Steps:")
			for _, item := range getPostChecklist() {
				h.Writer.Println(item)
			}
			resp, err := h.Writer.Confirm(v22_post_checklist_confirmation)
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
