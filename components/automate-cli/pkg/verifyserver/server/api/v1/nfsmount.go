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
		c.Next(&fiber.Error{Code: http.StatusBadRequest, Message: "Invalid Body Request"})
		return
	}
	if len(reqBody.AutomateNodeIPs) == 0 || len(reqBody.ChefInfraServerNodeIPs) == 0 || len(reqBody.PostgresqlNodeIPs) == 0 || len(reqBody.OpensearchNodeIPs) == 0 {
		c.Next(&fiber.Error{Code: http.StatusBadRequest, Message: "AutomateNodeIPs, ChefInfraServerNodeIPs, PostgresqlNodeIPs or OpensearchNodeIPs cannot be empty"})
		return
	}

	if reqBody.MountLocation == "" {
		c.Next(&fiber.Error{Code: http.StatusBadRequest, Message: "Mount Location cannot be empty"})
		return
	}
	h.Logger.Debug("Mount Location Recieved: ", reqBody.MountLocation)

	NfsMountDetails := h.NFSMountService.GetNFSMountDetails(reqBody)
	c.JSON(response.BuildSuccessResponse(NfsMountDetails))
}
