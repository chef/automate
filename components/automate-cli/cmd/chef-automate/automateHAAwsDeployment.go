package main

import (
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

func (a *awsDeployment) doProvisionJob(args []string) error {
	writer.Print("AWS Provision")
	err := bootstrapEnv(args)
	if err != nil {
		return err
	}
	writer.Printf("provisioning infra for automate HA \n\n\n\n")
	args = args[1:]
	args = append([]string{"-y"}, args...)
	return executeAutomateClusterCtlCommand("provision", args, provisionInfraHelpDocs)
}
