package v1

import (
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/constants"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/response"
	"github.com/gofiber/fiber"
)

func (h *Handler) GetStatus(c *fiber.Ctx) {
	services, err := h.StatusService.GetServices()
	if err != nil {
		c.JSON(response.BuildSuccessResponse(&models.StatusDetails{
			Status:   constants.OK,
			Services: &[]models.ServiceDetails{},
			Error:    err.Error(),
		}))
	} else {
		c.JSON(response.BuildSuccessResponse(&models.StatusDetails{
			Status:   constants.OK,
			Services: services,
		}))
	}
}
