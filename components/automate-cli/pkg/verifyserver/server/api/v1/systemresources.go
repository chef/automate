package v1

import (
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/response"
	"github.com/gofiber/fiber"
)

func (h *Handler) GetSystemResource(c *fiber.Ctx) {

	nodeType := c.Query("node_type", "")
	deploymentState := c.Query("deployment_state", "")
	service, err := h.SystemResourceService.GetSystemResourcesForDeployment(nodeType, deploymentState)
	if err != nil {
		//send bad_request or resource not found statust code
		h.Logger.Debug("error returned from service ", err)
		c.Next(err) //need to handle
		return
	}

	c.JSON(response.BuildSuccessResponse(service))
}
