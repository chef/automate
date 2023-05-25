package v1

import (
	"net/http"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/response"
	"github.com/gofiber/fiber/v2"
)

func (h *Handler) CheckExternalPostgresql(c *fiber.Ctx) error {
	req := new(models.ExternalPgRequest)
	if err := c.BodyParser(&req); err != nil {
		h.Logger.Error("External postgresql config request body parsing failed: %v", err)
		return fiber.NewError(fiber.StatusBadRequest, err.Error())
	}
	if len(req.PostgresqlInstanceUrl) == 0 ||
		len(req.PostgresqlInstancePort) == 0 ||
		len(req.PostgresqlSuperUserUserName) == 0 ||
		len(req.PostgresqlSuperUserPassword) == 0 ||
		len(req.PostgresqlDbUserUserName) == 0 ||
		len(req.PostgresqlDbUserPassword) == 0 ||
		len(req.PostgresqlRootCert) == 0 {
		return fiber.NewError(http.StatusBadRequest, "Request Parameters cannot be empty")

	}
	pgConnection, err := h.ExternalPostgresqlService.GetPgConnection(req)
	if err != nil {
		h.Logger.Error("Error while getting  Details: ", err)
		return fiber.NewError(fiber.StatusInternalServerError, err.Error())
	}
	return c.JSON(response.BuildSuccessResponse(pgConnection))
}
