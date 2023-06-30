package v1

import (
	"net/http"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/response"
	"github.com/gofiber/fiber/v2"
)

func (h *Handler) NFSMount(c *fiber.Ctx) error {
	reqBody := models.NFSMountRequest{}
	if err := c.BodyParser(&reqBody); err != nil {
		h.Logger.Error(err.Error())
		return fiber.NewError(http.StatusBadRequest, "Invalid Body Request")
	}

	if len(reqBody.AutomateNodeIPs) == 0 || len(reqBody.ChefInfraServerNodeIPs) == 0 || len(reqBody.PostgresqlNodeIPs) == 0 || len(reqBody.OpensearchNodeIPs) == 0 {
		return fiber.NewError(http.StatusBadRequest, "AutomateNodeIPs, ChefInfraServerNodeIPs, PostgresqlNodeIPs or OpensearchNodeIPs cannot be empty")
	}

	if reqBody.MountLocation == "" {
		return fiber.NewError(http.StatusBadRequest, "Mount Location cannot be empty")
	}
	h.Logger.Debug("Mount Location Recieved: ", reqBody.MountLocation)

	nfsMountDetails := h.NFSMountService.GetNFSMountDetails(reqBody)
	return c.JSON(response.BuildSuccessResponse(nfsMountDetails))
}

func (h *Handler) NFSMountLoc(c *fiber.Ctx) error {
	reqBody := models.NFSMountLocRequest{}
	if err := c.BodyParser(&reqBody); err != nil {
		h.Logger.Error(err.Error())
		return fiber.NewError(http.StatusBadRequest, "Invalid Body Request")
	}
	if reqBody.MountLocation == "" {
		return fiber.NewError(http.StatusBadRequest, "Mount Location cannot be empty")
	}
	nfsMountLoc := h.NFSMountService.GetNFSMountLoc(reqBody)
	if nfsMountLoc.Address == "" && nfsMountLoc.MountLocation != "" {
		return fiber.NewError(http.StatusNotFound, "Failed to get NFS mount location")
	}
	return c.JSON(response.BuildSuccessResponse(nfsMountLoc))
}
