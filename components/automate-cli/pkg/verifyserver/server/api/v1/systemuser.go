package v1

import (
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/response"
	"github.com/gofiber/fiber/v2"
)

func (h *Handler) CheckSystemUser(c *fiber.Ctx) error{
	checks := h.SystemUserService.GetSystemUserServiceDetails()
	return c.JSON(response.BuildSuccessResponse(checks))
}
