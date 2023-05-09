package v1

import (
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/response"
	"github.com/gofiber/fiber"
)

func (h *Handler) CheckSoftwareVersion(c *fiber.Ctx) {
	services,err := h.SoftwareVersionService.GetSoftwareVersionServices()
	if err != nil {
		c.Status(fiber.StatusBadRequest).JSON(err)
		return
	}
	c.JSON(response.BuildSuccessResponse(&models.SoftwareVersionDetails{
		Passed: services.Passed,
		Checks: services.Checks,
	}))
}
