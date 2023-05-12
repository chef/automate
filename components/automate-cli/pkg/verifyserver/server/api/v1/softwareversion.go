package v1

import (
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/response"
	"github.com/gofiber/fiber"
)

func (h *Handler) CheckSoftwareVersion(c *fiber.Ctx) {
	services, err := h.SoftwareVersionService.GetSoftwareVersionServices(c.Query("node_type"))
	if err != nil {
		c.Next(&fiber.Error{Message: err.Error(),Code: fiber.StatusBadRequest})
		return
	}
	c.JSON(response.BuildSuccessResponse(models.SoftwareVersionDetails{
		Passed: services.Passed,
		Checks: services.Checks,
	}))
}
