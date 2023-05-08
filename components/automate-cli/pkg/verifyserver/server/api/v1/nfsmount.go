package v1

import (
	"net/http"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/response"
	"github.com/gofiber/fiber"
)

func (h *Handler) NFSMount(c *fiber.Ctx) {
	reqBody := models.NFSMountRequest{}
	if err := c.BodyParser(&reqBody); err != nil {
		h.Logger.Error(err.Error())
		c.JSON(response.BuildFailedResponse(&fiber.Error{Code: http.StatusBadRequest, Message: "Invalid Body Request"}))
		return
	}
	if len(reqBody.AutomateNodeIPs) == 0 || len(reqBody.ChefInfraServerNodeIPs) == 0 || len(reqBody.PostgresqlNodeIPs) == 0 || len(reqBody.OpensearchNodeIPs) == 0 || reqBody.MountLocation == "" {
		c.JSON(response.BuildFailedResponse(&fiber.Error{Code: http.StatusBadRequest, Message: "Give all the required body parameters"}))
		return
	}

	NFSMountDetails := h.NFSMountService.GetNFSMountDetails(reqBody, false)
	c.JSON(response.BuildSuccessResponse(NFSMountDetails))
}
