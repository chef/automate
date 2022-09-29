package migratorv4

import (
	"fmt"

	"github.com/chef/automate/components/automate-deployment/pkg/cli"
	"github.com/chef/automate/lib/io/fileutils"
	"github.com/fatih/color"
	"github.com/pkg/errors"
)

type CheckDirExists struct {
	writer    *cli.Writer
	runError  error
	hasError  bool
	fileutils fileutils.FileUtils
}

func NewCheckDirExists(w *cli.Writer, fileutils fileutils.FileUtils) *CheckDirExists {
	return &CheckDirExists{
		writer:    w,
		fileutils: fileutils,
	}
}

func (cde *CheckDirExists) setError(err error) error {
	cde.runError = err
	cde.hasError = true
	return err
}

func (cde *CheckDirExists) Run() error {

	existDirData, err := cde.fileutils.PathExists(ELASTICSEARCH_DATA_DIR)
	if err != nil {
		err = errors.Wrap(err, "Failed to check directory "+ELASTICSEARCH_DATA_DIR)
		cde.setError(err)
		return err
	}
	existDirVar, err := cde.fileutils.PathExists(ELASTICSEARCH_VAR_DIR)
	if err != nil {
		err = errors.Wrap(err, "Failed to check directory "+ELASTICSEARCH_VAR_DIR)
		cde.setError(err)
		return err
	}
	if !existDirVar && !existDirData {
		err = errors.New(fmt.Sprintf("Directories not found :\n%s\n%s", ELASTICSEARCH_DATA_DIR, ELASTICSEARCH_VAR_DIR))
		cde.setError(err)
		return err
	}
	if !existDirData {
		err = errors.New("Directory not found : " + ELASTICSEARCH_DATA_DIR)
		cde.setError(err)
		return err
	}
	if !existDirVar {
		err = errors.New("Directory not found : " + ELASTICSEARCH_VAR_DIR)
		cde.setError(err)
		return err
	}
	return nil
}

func (cde *CheckDirExists) ErrorHandler() {
	if cde.hasError {
		cde.writer.Println("[" + color.New(color.FgRed).Sprint("Error") + "] " + cde.runError.Error())
	}
}
