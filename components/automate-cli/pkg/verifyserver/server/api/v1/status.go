package v1

import (
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/constants"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/response"
	"github.com/chef/automate/lib/version"
	"github.com/gofiber/fiber/v2"
)

func (h *Handler) GetStatus(c *fiber.Ctx) error {
	services, err := h.StatusService.GetServices()
	if err != nil {
		return c.JSON(response.BuildSuccessResponse(&models.StatusDetails{
			Status:     constants.OK,
			Services:   &[]models.ServiceDetails{},
			CliVersion: version.BuildTime,
			Error:      err.Error(),
		}))
	}

	return c.JSON(response.BuildSuccessResponse(&models.StatusDetails{
		Status:     constants.OK,
		Services:   services,
		CliVersion: version.BuildTime,
	}))
}
