package main

type deployManager interface {
	doDeployWork(args []string) error
	doProvisionJob(args []string) error
	generateConfig(state string) error
}

func getDeployer(configPath string) (deployManager, error) {
	deployerType, err := getModeFromConfig(configPath)
	if err != nil {
		return nil, err
	}
	if deployerType == AWS_MODE {
		return newAwsDeployemnt(configPath), nil
	}
	if deployerType == EXISTING_INFRA_MODE {
		return newExistingInfa(configPath), nil
	}
	if deployerType == HA_MODE {
		return newHaWithoutConfig(), nil
	}
	return nil, nil
}
