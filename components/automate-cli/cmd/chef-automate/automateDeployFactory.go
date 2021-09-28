package main

type deployManager interface {
	doDeployWork(args []string) error
	doProvisionJob(args []string) error
}

func getDeployer(args []string) (deployManager, error) {
	deployerType, err := getModeFromConfig(args)
	if err != nil {
		return nil, err
	}
	if deployerType == AWS_MODE {
		return newAwsDeployemnt(), nil
	}
	if deployerType == EXISTING_INFRA_MODE {
		return newExistingInfa(), nil
	}
	if deployerType == HA_MODE {
		return newHaWithoutConfig(), nil
	}
	return nil, nil
}
