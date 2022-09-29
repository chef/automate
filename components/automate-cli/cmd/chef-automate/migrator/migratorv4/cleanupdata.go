package migratorv4

import (
	"fmt"
	"time"

	"github.com/briandowns/spinner"
	"github.com/chef/automate/components/automate-deployment/pkg/cli"
	"github.com/chef/automate/lib/io/fileutils"
	"github.com/chef/automate/lib/majorupgrade_utils"
	"github.com/fatih/color"
	"github.com/pkg/errors"
)

const (
	fcleanUpScript = `
rm -rf %[1]v/svc/automate-opensearch/data.os;
rm -rf %[1]v/svc/automate-opensearch/var.os;
rm -rf %[1]v/svc/automate-elasticsearch/data;
rm -rf %[1]v/svc/automate-elasticsearch/var;
`
	CLEANUP_ID = "clean_up"
)

type Cleanup struct {
	writer         *cli.Writer
	utils          MigratorV4Utils
	fileutils      fileutils.FileUtils
	spinner        *spinner.Spinner
	autoAccept     bool
	forceExecute   bool
	spinnerTimeout time.Duration
}

func NewCleanUp(w *cli.Writer, utils MigratorV4Utils, fileutils fileutils.FileUtils, autoAccept, forceExecute bool, spinnerTimeout time.Duration) *Cleanup {
	return &Cleanup{
		writer:         w,
		utils:          utils,
		fileutils:      fileutils,
		autoAccept:     autoAccept,
		forceExecute:   forceExecute,
		spinnerTimeout: spinnerTimeout,
	}
}

func (cu *Cleanup) Clean(skipConfirmation bool) error {
	err := cu.startCleanup(cu.forceExecute, cu.autoAccept, skipConfirmation)
	if err != nil {
		cu.showClearDataFailedMessage()
		return err
	}
	cu.showClearDataSuccessMessage()
	return nil
}

func (cu *Cleanup) startCleanup(forceExecute, autoAccept, skipConfirmation bool) error {
	habRoot := cu.fileutils.GetHabRootPath()
	isExecuted, err := cu.utils.ReadV4Checklist(CLEANUP_ID, habRoot+majorupgrade_utils.UPGRADE_METADATA)
	if err != nil {
		return err
	}

	if isExecuted {
		if forceExecute {
			isExecuted = false
		} else {
			err := cu.askForConfirmation(`Your have already deleted your old Elasticsearch data.
Do you want to perform clean up again?`)
			if err != nil {
				return err
			} else {
				isExecuted = false
			}
		}
	}

	if !isExecuted {
		err := cu.runcleanUpes(autoAccept, skipConfirmation)
		if err != nil {
			return err
		}
	}
	return nil
}

func (cu *Cleanup) runcleanUpes(autoAccept, skipConfirmation bool) error {
	if !autoAccept && !skipConfirmation {
		err := cu.askForConfirmation(`Would you like to clean up the old Elasticsearch data now?`)
		if err != nil {
			return err
		}
	}
	cu.showDeletingMessage()
	habRoot := cu.fileutils.GetHabRootPath()
	cleanUpScript := fmt.Sprintf(fcleanUpScript, habRoot)
	args := []string{
		"-c",
		cleanUpScript,
	}
	err := cu.utils.ExecuteCommand("/bin/sh", args, "")
	if err != nil {
		return err
	} else {
		habRoot := cu.fileutils.GetHabRootPath()
		err = cu.utils.UpdatePostChecklistFile(CLEANUP_ID, habRoot+majorupgrade_utils.UPGRADE_METADATA)
		if err != nil {
			return err
		}
	}
	return nil
}

func (cu *Cleanup) showDeletingMessage() {
	cu.spinner = cu.writer.NewSpinner()
	cu.spinner.Suffix = "  Clean up in progress"
	cu.spinner.Start()
	time.Sleep(cu.spinnerTimeout)
}

func (cu *Cleanup) showClearDataSuccessMessage() {
	if cu.spinner == nil {
		return
	}
	cu.spinner.FinalMSG = " " + color.New(color.FgGreen).Sprint("✔") + "  Clean up successful"
	cu.spinner.Stop()
	cu.writer.Println("")
}

func (cu *Cleanup) showClearDataFailedMessage() {
	if cu.spinner == nil {
		return
	}
	cu.spinner.FinalMSG = " " + color.New(color.FgRed).Sprint("✖") + "  Clean up failed"
	cu.spinner.Stop()
	cu.writer.Println("")
}

func (cu *Cleanup) askForConfirmation(message string) error {
	res, err := cu.writer.Confirm(message)
	if err != nil {
		return err
	}
	if !res {
		return errors.New("Cleanup Process Terminated.")
	}
	return nil
}
