package v1

import (
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/response"
	"github.com/gofiber/fiber"
)

func (h *Handler) GetStatus(c *fiber.Ctx) {
	services, err := h.StatusService.GetServices()
	if err != nil {
		c.Next(err)
		return
	}
	c.JSON(response.BuildSuccessResponse(&models.StatusDetails{
		Status:   "OK",
		Services: services,
	}))
}
