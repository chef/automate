package v1

import (
	"errors"
	"fmt"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/constants"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/response"
	"github.com/gofiber/fiber/v2"
)

func getNodeTypeMap() map[string]constants.NodeType {
	return map[string]constants.NodeType{
		string(constants.NodeTypeAutomate):   constants.NodeTypeAutomate,
		string(constants.NodeTypeChefServer): constants.NodeTypeChefServer,
		string(constants.NodeTypePostgresql): constants.NodeTypePostgresql,
		string(constants.NodeTypeOpensearch): constants.NodeTypeOpensearch,
		string(constants.NodeTypeBastion):    constants.NodeTypeBastion,
	}
}

func getNodeTypeOptions() string {
	options := ""
	optionsMap := getNodeTypeMap()

	for option, _ := range optionsMap {
		options = options + option + ","
	}
	return options[:len(options)-1]
}

func getNodeType(nodeType string) (constants.NodeType, error) {
	value, present := getNodeTypeMap()[nodeType]

	if !present {
		return constants.NodeType(""), errors.New("Invalid NodeType,valid value can be " + getNodeTypeOptions())
	}
	return value, nil
}

func getDeploymentStateMap() map[string]constants.DeploymentState {
	return map[string]constants.DeploymentState{
		string(constants.DeploymentStatePreDeploy):  constants.DeploymentStatePreDeploy,
		string(constants.DeploymentStatePostDeploy): constants.DeploymentStatePostDeploy,
	}
}

func getDeploymentState(deploymentState string) (constants.DeploymentState, error) {
	value, present := getDeploymentStateMap()[deploymentState]

	if !present {
		return constants.DeploymentState(""), errors.New("Invalid DeploymentState,valid value can be " + getDeploymentStateOptions())
	}
	return value, nil
}

func getDeploymentStateOptions() string {
	options := ""
	optionsMap := getDeploymentStateMap()

	for option, _ := range optionsMap {
		options = options + option + ","
	}
	return options[:len(options)-1]
}

func (h *Handler) GetSystemResource(c *fiber.Ctx) error {

	nodeType, err := getNodeType(c.Query("node_type"))

	if err != nil {
		return fiber.NewError(fiber.StatusBadRequest, fmt.Sprintf(`Unsupported query or missing query. Expected values for query 'node_type' are %v,%v,%v,%v,%v`, constants.NodeTypeAutomate, constants.NodeTypeChefServer, constants.NodeTypePostgresql, constants.NodeTypeOpensearch, constants.NodeTypeBastion))
	}

	deploymentState, err := getDeploymentState(c.Query("deployment_state"))

	if err != nil {
		return fiber.NewError(fiber.StatusBadRequest, fmt.Sprintf(`Unsupported query or missing query. Expected values for query 'deployment_state' are %s,%s`, constants.DeploymentStatePreDeploy, constants.DeploymentStatePostDeploy))
	}

	service := h.SystemResourceService.GetSystemResourcesForDeployment(nodeType, deploymentState)
	return c.JSON(response.BuildSuccessResponse(service))
}
