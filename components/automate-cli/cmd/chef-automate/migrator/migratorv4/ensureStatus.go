package migratorV4

import (
	"errors"
	"fmt"

	"github.com/chef/automate/components/automate-deployment/pkg/cli"
	"github.com/fatih/color"
)

type EnsureStatusInspection struct {
	writer          *cli.Writer
	migratorUtils   MigratorV4Utils
	exitError       error
	exitedWithError bool
}

func NewEnsureStatus(w *cli.Writer, migratorUtils MigratorV4Utils) *EnsureStatusInspection {
	return &EnsureStatusInspection{
		writer:        w,
		migratorUtils: migratorUtils,
	}
}

func (es *EnsureStatusInspection) Run() (err error) {
	status, err := es.migratorUtils.GetServicesStatus()
	if err != nil {
		es.setError(err)
		return err
	}
	if !status {
		err = errors.New("Please make sure all services are healthy by running " + color.New(color.Bold).Sprint("chef-automate status"))
		es.setError(err)
		return err
	}
	return nil
}

func (es *EnsureStatusInspection) setError(err error) error {
	es.exitError = err
	es.exitedWithError = true
	return err
}

func (es *EnsureStatusInspection) ErrorHandler() {
	if es.exitedWithError {
		es.writer.Println(fmt.Errorf("["+color.New(color.FgRed).Sprint("Error")+"] %w", es.exitError).Error())
	}
}
