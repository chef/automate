package v1

import (
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/response"
	"github.com/gofiber/fiber"
)

func (h *Handler) CheckExternalPostgresql(c *fiber.Ctx) {
	externalPostgresqlRequest := new(models.ExternalPgRequest)
	if err := c.BodyParser(&externalPostgresqlRequest); err != nil {
		h.Logger.Error("External postgresql config request body parsing failed: %v", err.Error())
		c.Next(&fiber.Error{Code: fiber.StatusBadRequest, Message: err.Error()})
		return
	}
	pgConnection, err := h.ExternalPostgresqlService.GetPgConnection(externalPostgresqlRequest)
	if err != nil {
		h.Logger.Error("Error while getting  Details: ", err)
		c.Next(&fiber.Error{Message: err.Error(), Code: fiber.StatusInternalServerError})
		return
	}
	c.JSON(response.BuildSuccessResponse(pgConnection))
}
