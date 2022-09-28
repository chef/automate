package migratorV4

import (
	"fmt"
	"time"

	"github.com/briandowns/spinner"
	"github.com/chef/automate/components/automate-deployment/pkg/cli"
	"github.com/fatih/color"
)

const (
	fcleanUpScript = `
	rm -rf %[1]vsvc/automate-opensearch/data.os;
	rm -rf %[1]vsvc/automate-opensearch/var.os;
	rm -rf %[1]vsvc/automate-elasticsearch/data;
	rm -rf %[1]vsvc/automate-elasticsearch/var;
	`
	habrootcmd = "HAB_LICENSE=accept-no-persist hab pkg path chef/deployment-service"
	CLEANUP_ID = "clean_up"
)

type Cleanup struct {
	writer       *cli.Writer
	utils        MigratorV4Utils
	runError     error
	hasError     bool
	spinner      *spinner.Spinner
	autoAccept   bool
	forceExecute bool
}

func NewCleanUp(w *cli.Writer, utils MigratorV4Utils, autoAccept, forceExecute bool) *Cleanup {
	return &Cleanup{
		writer:       w,
		utils:        utils,
		autoAccept:   autoAccept,
		forceExecute: forceExecute,
	}
}

func (cu *Cleanup) Run() error {
	err := cu.startCleanup(cu.forceExecute, cu.autoAccept)
	if err != nil {
		cu.setError(err)
		cu.showClearDataFailedMessage()
	}
	cu.showClearDataSuccessMessage()
	return nil
}

func (cu *Cleanup) ErrorHandler() {
	if cu.hasError {
		cu.writer.Println(cu.runError.Error())
	}
}

func (cu *Cleanup) Skip() error {
	return nil
}

func (cu *Cleanup) DefferedHandler() error {
	return nil
}

func (cs *Cleanup) setError(err error) error {
	cs.runError = err
	cs.hasError = true
	return err
}

func (cu *Cleanup) startCleanup(forceExecute, autoAccept bool) error {

	isExecuted, err := cu.utils.ReadV4Checklist(CLEANUP_ID)
	if err != nil {
		cu.setError(err)
		return err
	}

	if isExecuted {
		if forceExecute {
			isExecuted = false
		} else {
			err := cu.AskForConfirmation(`Your have already deleted your old Elasticsearch data.
Do you want to perform clean up again?`)
			if err != nil {
				cu.setError(err)
				return err
			} else {
				isExecuted = false
			}
		}
	}

	if !isExecuted {
		err := cu.runcleanUpes(autoAccept)
		if err != nil {
			cu.setError(err)
			return err
		}
	}
	return nil
}

func (cu *Cleanup) runcleanUpes(autoAccept bool) error {

	if !autoAccept {
		err := cu.AskForConfirmation(`Your old data will be cleaned-up
		Press y to continue and n to Exit`)
		if err != nil {
			return err
		}
	}
	cu.showDeletingMessage()
	habRoot := cu.utils.GetHabRootPath(habrootcmd)
	cleanUpScript := fmt.Sprintf(fcleanUpScript, habRoot)
	args := []string{
		"-c",
		cleanUpScript,
	}
	err := cu.utils.ExecuteCommand("/bin/sh", args, "")
	if err != nil {
		cu.setError(err)
		return err
	} else {
		err = cu.utils.UpdatePostChecklistFile(CLEANUP_ID)
		if err != nil {
			cu.setError(err)
			return err
		}
	}
	return nil
}

func (cu *Cleanup) showDeletingMessage() {
	cu.spinner = cu.writer.NewSpinner()
	cu.spinner.Suffix = "  Clean up in progres"
	cu.spinner.Start()
	time.Sleep(time.Second)
}

func (cu *Cleanup) showClearDataSuccessMessage() {
	cu.spinner.FinalMSG = " " + color.New(color.FgGreen).Sprint("✔") + "  Clean up successful"
	cu.spinner.Stop()
	cu.writer.Println("")
}

func (cu *Cleanup) showClearDataFailedMessage() {
	cu.spinner.FinalMSG = " " + color.New(color.FgRed).Sprint("✖") + "  Clean up failed"
	cu.spinner.Stop()
	cu.writer.Println("")
}

func (cu *Cleanup) AskForConfirmation(message string) error {
	res, err := cu.writer.Confirm(message)
	if err != nil {
		return err
	}
	if !res {
		return nil
	}
	return nil
}
