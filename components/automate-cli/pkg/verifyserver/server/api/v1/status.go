package v1

import (
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/response"
	"github.com/gofiber/fiber"
)

func (h *Handler) GetStatus(c *fiber.Ctx) {
	services, _ := h.StatusService.GetServices()
	c.JSON(response.BuildSuccessResponse(&models.StatusDetails{
		Status:   "ok",
		Services: services,
	}))
}
