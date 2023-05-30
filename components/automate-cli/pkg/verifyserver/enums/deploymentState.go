package enums

import (
	"errors"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/constants"
)

type DeploymentState string

const (
	DeploymentStatePreDeploy  DeploymentState = constants.PRE_DEPLOY
	DeploymentStatePostDeploy DeploymentState = constants.POST_DEPLOY
)

func getDeploymentStateMap() map[string]DeploymentState {
	return map[string]DeploymentState{
		string(DeploymentStatePreDeploy):  DeploymentStatePreDeploy,
		string(DeploymentStatePostDeploy): DeploymentStatePostDeploy,
	}
}

func getDeploymentStateOptions() string {
	options := ""
	optionsMap := getDeploymentStateMap()

	for option, _ := range optionsMap {
		options = options + option + ","
	}
	return options[:len(options)-1]
}

func GetDeploymentState(deploymentState string) (DeploymentState, error) {
	value, present := getDeploymentStateMap()[deploymentState]

	if !present {
		return DeploymentState(""), errors.New("Invalid DeploymentState,valid value can be " + getDeploymentStateOptions())
	}
	return value, nil
}
