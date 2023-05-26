package v1

import (
	"fmt"
	"net/http"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/constants"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/response"
	"github.com/gofiber/fiber/v2"
)


func (h *Handler) GetSystemResource(c *fiber.Ctx) error {
	nodeType := c.Query("node_type")
	deploymentState := c.Query("deployment_state")

	if nodeType != constants.AUTOMATE &&
		nodeType != constants.CHEF_INFRA_SERVER &&
		nodeType != constants.POSTGRESQL &&
		nodeType != constants.OPENSEARCH &&
		nodeType != constants.BASTION {

		return fiber.NewError(fiber.StatusBadRequest, fmt.Sprintf(`Unsupported query or missing query. Expected values for query 'node_type' are %s,%s,%s,%s,%s`, constants.AUTOMATE, constants.CHEF_INFRA_SERVER, constants.POSTGRESQL, constants.OPENSEARCH, constants.BASTION))
	}

	if deploymentState != constants.PRE_DEPLOY && deploymentState != constants.POST_DEPLOY {
		return fiber.NewError(fiber.StatusBadRequest, fmt.Sprintf(`Unsupported query or missing query. Expected values for query 'deployment_state' are %s,%s`, constants.PRE_DEPLOY, constants.POST_DEPLOY))
	}

	service, err := h.SystemResourceService.GetSystemResourcesForDeployment(nodeType, deploymentState)
	if err != nil {
		h.Logger.Debug("error returned from service ", err)
		return fiber.NewError(http.StatusBadRequest, err.Error())
	}
	return c.JSON(response.BuildSuccessResponse(service))
}
