package migratorV4

import (
	"fmt"
	"os/exec"
	"time"

	"github.com/briandowns/spinner"
	"github.com/chef/automate/components/automate-deployment/pkg/cli"
	"github.com/chef/automate/lib/io/fileutils"
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
)

type Cleanup struct {
	writer           *cli.Writer
	utils            MigratorV4Utils
	runError         error
	hasError         bool
	fileutils        fileutils.FileUtils
	spinner          *spinner.Spinner
	migrationConsent bool
	autoAccept       bool
	forceExecute     bool
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
	cu.showClearDataSucessMessage()
	err := cu.startCleanup(cu.forceExecute, cu.autoAccept)
	if err != nil {
		cu.showClearDataFailedMessage()
	}
	cu.showClearDataSucessMessage()
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
	m := NewMigratorV4Utils()

	isExecuted, err := m.ReadV4Checklist(CLEANUP_ID)
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
	habRoot := cu.utils.GetHabRootPath(habrootcmd)
	cleanUpScript := fmt.Sprintf(fcleanUpScript, habRoot)
	command := exec.Command("/bin/sh", "-c", cleanUpScript)
	err := command.Run()
	if err != nil {
		cu.setError(err)
		return err
	} else {
		m := NewMigratorV4Utils()
		if err != nil {
			cu.setError(err)
			return err
		}
		err = m.UpdatePostChecklistFile(CLEANUP_ID)
		if err != nil {
			cu.setError(err)
			return err
		}
		cu.showClearDataSucessMessage()
	}
	return nil
}

func (cu *Cleanup) showDeletingMessage() {
	cu.spinner = cu.writer.NewSpinner()
	cu.spinner.Suffix = fmt.Sprintf("  Clean up in progres")
	time.Sleep(time.Second)
}

func (cu *Cleanup) showClearDataSucessMessage() {
	cu.spinner.FinalMSG = " " + color.New(color.FgGreen).Sprint("✔") + "  Clean up failed"
	cu.spinner.Stop()
	cu.writer.Println("")
}

func (cu *Cleanup) showClearDataFailedMessage() {
	cu.spinner.FinalMSG = " " + color.New(color.FgGreen).Sprint("✔") + "  Clean up successful"
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
	cu.migrationConsent = res
	return nil
}
