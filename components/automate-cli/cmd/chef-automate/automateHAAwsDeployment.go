package main

import (
	"os"

	"github.com/chef/automate/components/automate-cli/pkg/status"
)

type awsDeployment struct{}

func newAwsDeployemnt() *awsDeployment {
	return &awsDeployment{}
}

func (a *awsDeployment) doDeployWork(args []string) error {
	if isA2HARBFileExist() {
		err := deployA2HA(args)
		return err
	} else {
		return status.New(status.InvalidCommandArgsError, errProvisonInfra)
	}
}

func checkIfFileExist(path string) bool {
	if _, err := os.Stat(path); os.IsNotExist(err) {
		return false
	} else {
		return true
	}
}
