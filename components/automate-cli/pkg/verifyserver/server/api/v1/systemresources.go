package v1

import (
	"fmt"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/enums"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/response"
	"github.com/gofiber/fiber/v2"
)

func (h *Handler) GetSystemResource(c *fiber.Ctx) error {

	nodeType, err := enums.GetNodeType(c.Query("node_type"))

	if err != nil {
		return fiber.NewError(fiber.StatusBadRequest, fmt.Sprintf(`Unsupported query or missing query. Expected values for query 'node_type' are %v,%v,%v,%v,%v`, enums.NodeTypeAutomate, enums.NodeTypeChefServer, enums.NodeTypePostgresql, enums.NodeTypeOpensearch, enums.NodeTypeBastion))
	}

	deploymentState, err := enums.GetDeploymentState(c.Query("deployment_state"))

	if err != nil {
		return fiber.NewError(fiber.StatusBadRequest, fmt.Sprintf(`Unsupported query or missing query. Expected values for query 'deployment_state' are %s,%s`, enums.DeploymentStatePreDeploy, enums.DeploymentStatePostDeploy))
	}

	service := h.SystemResourceService.GetSystemResourcesForDeployment(nodeType, deploymentState)
	return c.JSON(response.BuildSuccessResponse(service))
}
