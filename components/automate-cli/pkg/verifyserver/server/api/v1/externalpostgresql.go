package v1

import (
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/response"
	"github.com/gofiber/fiber/v2"
)

func (h *Handler) CheckExternalPostgresql(c *fiber.Ctx) error {
	externalPostgresqlRequest := new(models.ExternalPgRequest)
	if err := c.BodyParser(&externalPostgresqlRequest); err != nil {
		h.Logger.Error("External postgresql config request body parsing failed: %v", err)
		return fiber.NewError(fiber.StatusBadRequest, err.Error())
	}
	pgConnection, err := h.ExternalPostgresqlService.GetPgConnection(externalPostgresqlRequest)
	if err != nil {
		h.Logger.Error("Error while getting  Details: ", err)
		return fiber.NewError(fiber.StatusInternalServerError, err.Error())
	}
	return c.JSON(response.BuildSuccessResponse(pgConnection))
}
