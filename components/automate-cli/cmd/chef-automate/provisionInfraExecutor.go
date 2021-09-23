package main

type provisionInfraManager interface {
	doProvisionJob(args []string) error
}

func getProvisioner(args []string) provisionInfraManager {
	deployerType := getModeFromConfig(args)
	if deployerType == AWS_MODE {
		return &awsDeployment{}
	}
	if deployerType == HA_MODE {
		return &provisonWithoutConfig{}
	}
	return nil
}
