package v1

import (
	"net/http"
	"strings"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/constants"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/response"
	"github.com/gofiber/fiber/v2"
)

func (h *Handler) ExternalOpensearch(c *fiber.Ctx) error {
	reqBody := models.ExternalOS{}
	if err := c.BodyParser(&reqBody); err != nil {
		h.Logger.Error(err.Error())
		return fiber.NewError(http.StatusBadRequest, "Invalid Body Request")
	}

	if strings.TrimSpace(reqBody.OSDomainName) == "" ||
		strings.TrimSpace(reqBody.OSDomainURL) == "" ||
		strings.TrimSpace(reqBody.OSUsername) == "" ||
		strings.TrimSpace(reqBody.OSUserPassword) == "" ||
		strings.TrimSpace(reqBody.OSCert) == "" {
		return fiber.NewError(http.StatusBadRequest, "OSDomainName, OSDomainURL, OSUsername, OSUserPassword or OSCert cannot be empty")
	}

	externalOpensearchResult := h.ExternalOpensearchService.GetExternalOpensearchDetails(reqBody, constants.HTTPS_PORT)
	return c.JSON(response.BuildSuccessResponse(externalOpensearchResult))
}
