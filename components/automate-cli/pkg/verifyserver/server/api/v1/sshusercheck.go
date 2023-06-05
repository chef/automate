package v1

import (
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/response"
	"github.com/gofiber/fiber/v2"
)

func (h *Handler) CheckSshUser(c *fiber.Ctx) error {
	req := new(models.SshUserChecksRequest)
	if err := c.BodyParser(&req); err != nil {
		h.Logger.Error("Ssh User Check request body parsing failed: ", err)
		return fiber.NewError(fiber.StatusBadRequest, err.Error())
	}

	if len(req.Ip) == 0 ||
		len(req.User_Name) == 0 ||
		len(req.Private_Key) == 0 ||
		len(req.Port) == 0 {
		return fiber.NewError(fiber.StatusBadRequest, `Request Parameters that is 'ip', 'user_name', 'ssh_port' and 'private_key' cannot be empty`)
	}

	service, err := h.SshUserCheckService.CheckSshUserDetails(req)
	if err != nil {
		h.Logger.Error("Error while checking the SSH details: ", err)
		return fiber.NewError(fiber.StatusInternalServerError, err.Error())
	}
	return c.JSON(response.BuildSuccessResponse(service))
}
