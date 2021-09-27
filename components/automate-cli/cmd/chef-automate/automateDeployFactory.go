package main

type deployManager interface {
	doDeployWork(args []string) error
	doProvisionJob(args []string) error
}

func getDeployer(args []string) deployManager {
	deployerType := getModeFromConfig(args)
	if deployerType == AWS_MODE {
		return newAwsDeployemnt()
	}
	if deployerType == EXISTING_INFRA_MODE {
		return newExistingInfa()
	}
	if deployerType == HA_MODE {
		return newHaWithoutConfig()
	}
	return nil
}
