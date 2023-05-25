package v1

import (
	"fmt"
	"net/http"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/constants"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/response"
	"github.com/gofiber/fiber/v2"
)

const (
	nodeType        = "node_type"
	deploymentState = "deployment_state"
)

func (h *Handler) GetSystemResource(c *fiber.Ctx) error {
	nodeQuery := c.Query(nodeType, "")
	deploymentQuery := c.Query(deploymentState, "")

	if nodeQuery != constants.AUTOMATE &&
		nodeQuery != constants.CHEF_INFRA_SERVER &&
		nodeQuery != constants.POSTGRESQL &&
		nodeQuery != constants.OPENSEARCH &&
		nodeQuery != constants.BASTION {

		return fiber.NewError(fiber.StatusBadRequest, fmt.Sprintf(`Unsupported query or missing query. Expected values for query 'node_type' are %s,%s,%s,%s,%s`, constants.AUTOMATE, constants.CHEF_INFRA_SERVER, constants.POSTGRESQL, constants.OPENSEARCH, constants.BASTION))
	}

	if deploymentQuery != constants.PRE_DEPLOY && deploymentQuery != constants.POST_DEPLOY {
		return fiber.NewError(fiber.StatusBadRequest, fmt.Sprintf(`Unsupported query or missing query. Expected values for query 'deployment_state' are %s,%s`, constants.PRE_DEPLOY, constants.POST_DEPLOY))
	}

	service, err := h.SystemResourceService.GetSystemResourcesForDeployment(nodeQuery, deploymentQuery)
	if err != nil {
		h.Logger.Debug("error returned from service ", err)
		return fiber.NewError(http.StatusBadRequest, err.Error())
	}
	return c.JSON(response.BuildSuccessResponse(service))
}
