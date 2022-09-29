package migratorv4

import (
	"errors"
	"fmt"

	"github.com/chef/automate/components/automate-deployment/pkg/cli"
	"github.com/chef/automate/lib/io/fileutils"
	"github.com/fatih/color"
)

type CheckStorage struct {
	writer    *cli.Writer
	utils     MigratorV4Utils
	runError  error
	hasError  bool
	fileutils fileutils.FileUtils
}

func NewCheckStorage(w *cli.Writer, utils MigratorV4Utils, fileutils fileutils.FileUtils) *CheckStorage {
	return &CheckStorage{
		writer:    w,
		utils:     utils,
		fileutils: fileutils,
	}
}

func (cs *CheckStorage) setError(err error) error {
	cs.runError = err
	cs.hasError = true
	return err
}

func (cs *CheckStorage) Run() error {
	esDirSize, err := cs.fileutils.CalDirSizeInGB(ELASTICSEARCH_DATA_DIR)
	if err != nil {
		cs.setError(err)
		return err
	}
	minimumRequiredSpace := esDirSize * 110 / 100
	osDirSize, err := cs.fileutils.GetFreeSpaceinGB(OPENSEARCH_DATA_DIR)
	if err != nil {
		cs.setError(err)
		return err
	}
	if osDirSize < minimumRequiredSpace {
		err = errors.New(fmt.Sprintf("Insufficient space. Directory %s should have minimum %.2fGB space available.", OPENSEARCH_DATA_DIR, minimumRequiredSpace))
		cs.setError(err)
		return err
	}
	return nil
}

func (cs *CheckStorage) ErrorHandler() {
	if cs.hasError {
		cs.writer.Println("[" + color.New(color.FgRed).Sprint("Error") + "] " + cs.runError.Error())
	}
}
