package main

type deployManager interface {
	doDeployWork(args []string) error
	doProvisionJob(args []string) error
}

func getDeployer(args []string) deployManager {
	deployerType := getModeFromConfig(args)
	if deployerType == AWS_MODE {
		return &awsDeployment{}
	}
	if deployerType == EXISTING_INFRA_MODE {
		return &existingInfra{}
	}
	if deployerType == HA_MODE {
		return &haWithoutConfig{}
	}
	return nil
}
