package v1

import (
	"fmt"
	"net/http"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/constants"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/response"
	"github.com/gofiber/fiber"
)

const (
	nodeType        = "node_type"
	deploymentState = "deployment_state"
)

func (h *Handler) GetSystemResource(c *fiber.Ctx) {
	nodeQuery := c.Query(nodeType, "")
	deploymentQuery := c.Query(deploymentState, "")

	if nodeQuery != constants.AUTOMATE &&
		nodeQuery != constants.CHEF_INFRA_SERVER &&
		nodeQuery != constants.POSTGRESQL &&
		nodeQuery != constants.OPENSEARCH &&
		nodeQuery != constants.BASTION {

		c.Next(&fiber.Error{
			Message: fmt.Sprintf(`Unsupported query or missing query. Expected values for query 'node_type' are %s,%s,%s,%s,%s`, constants.AUTOMATE, constants.CHEF_INFRA_SERVER, constants.POSTGRESQL, constants.OPENSEARCH, constants.BASTION),
			Code:    fiber.StatusBadRequest,
		})
		return
	}

	if deploymentQuery != constants.PRE_DEPLOY && deploymentQuery != constants.POST_DEPLOY {
		c.Next(&fiber.Error{
			Message: fmt.Sprintf(`Unsupported query or missing query. Expected values for query 'deployment_state' are %s,%s`, constants.PRE_DEPLOY, constants.POST_DEPLOY),
			Code:    fiber.StatusBadRequest,
		})
		return
	}

	service, err := h.SystemResourceService.GetSystemResourcesForDeployment(nodeQuery, deploymentQuery)
	if err != nil {
		h.Logger.Debug("error returned from service ", err)
		c.Next(&fiber.Error{
			Code:    http.StatusBadRequest,
			Message: err.Error(),
		})
		return
	}
	c.JSON(response.BuildSuccessResponse(service))
}
