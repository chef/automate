package v1

import (
	"net/http"
	"strings"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/response"
	"github.com/gofiber/fiber/v2"
)

func (h *Handler) ExternalOpensearch(c *fiber.Ctx) error {
	reqBody := models.ExternalOSRequest{}
	if err := c.BodyParser(&reqBody); err != nil {
		h.Logger.Error(err.Error())
		return fiber.NewError(http.StatusBadRequest, "Invalid Body Request")
	}

	if strings.TrimSpace(reqBody.OSDomainName) == "" ||
		strings.TrimSpace(reqBody.OSDomainURL) == "" ||
		strings.TrimSpace(reqBody.OSUsername) == "" ||
		strings.TrimSpace(reqBody.OSUserPassword) == "" ||
		strings.TrimSpace(reqBody.OSCert) == "" {
		h.Logger.Error("opensearch_domain_name, opensearch_domain_url, opensearch_username, opensearch_user_password or opensearch_root_cert cannot be empty")
		return fiber.NewError(http.StatusBadRequest, "opensearch_domain_name, opensearch_domain_url, opensearch_username, opensearch_user_password or opensearch_root_cert cannot be empty")
	}

	externalOpensearchResult := h.ExternalOpensearchService.GetExternalOpensearchDetails(reqBody)
	return c.JSON(response.BuildSuccessResponse(externalOpensearchResult))
}
