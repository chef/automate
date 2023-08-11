package licenseaudit

import (
	"bytes"
	"os/exec"

	"github.com/chef/automate/lib/logger"
)

type ExecuteCommand interface {
	Execute(command string) (string, error)
}

type Execute struct {
	log logger.Logger
}

func NewExecute(log logger.Logger) *Execute {
	return &Execute{log: log}
}

func (e Execute) Execute(command string) (string, error) {

	e.log.Info("Inside the  Execute method for string---------")

	//Executing command
	cmd := exec.Command("/bin/sh", "-c", command)

	var outputBuffer, errorBuffer bytes.Buffer
	cmd.Stdout = &outputBuffer
	cmd.Stderr = &errorBuffer

	//Executing the command
	err := cmd.Run()
	if err != nil {
		return outputBuffer.String(), err
	}

	e.log.Infof("Got the output from the license command as %s with error string as %s", outputBuffer.String(), errorBuffer.String())

	return outputBuffer.String(), nil
}
