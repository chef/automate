package v1

import (
	"net/http"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/response"
	"github.com/gofiber/fiber"
)

func (h *Handler) GetSystemResource(c *fiber.Ctx) {

	nodeType := c.Query("node_type", "")
	deploymentState := c.Query("deployment_state", "")
	service, err := h.SystemResourceService.GetSystemResourcesForDeployment(nodeType, deploymentState)
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
