package main

type awsProvision struct{}

func newAwsProvision() *awsDeployment {
	return &awsDeployment{}
}

func (a *awsDeployment) doProvisionJob(args []string) error {
	writer.Print("AWS Provision")
	err := bootstrapEnv(args)
	if err != nil {
		return err
	}
	writer.Printf("provisioning infra for automate HA \n\n\n\n")
	args = args[1:]
	return executeAutomateClusterCtlCommand("provision", args, provisionInfraHelpDocs)
}
