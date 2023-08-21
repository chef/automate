package licenseaudit

import (
	"bytes"
	"fmt"
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

	//Executing command
	cmd := exec.Command("/bin/sh", "-c", command)

	var outputBuffer, errorBuffer bytes.Buffer
	cmd.Stdout = &outputBuffer
	cmd.Stderr = &errorBuffer

	//Executing the command
	err := cmd.Run()
	if err != nil {
		return errorBuffer.String(), err
	}

	output := fmt.Sprintf("%s %s", outputBuffer.String(), errorBuffer.String())

	e.log.Debugf("Got the output from the license command %s as %s", command, output)

	return outputBuffer.String(), nil
}
