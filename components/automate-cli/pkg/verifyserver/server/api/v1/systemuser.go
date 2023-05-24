package v1

import (
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/response"
	"github.com/gofiber/fiber"
)

func (h *Handler) CheckSystemUser(c *fiber.Ctx) {
	checks := h.SystemUserService.GetSystemUserServiceDetails()
	c.JSON(response.BuildSuccessResponse(checks))
}
