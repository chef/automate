package v1

import (
	"net/http"
	"strings"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/response"
	"github.com/gofiber/fiber/v2"
)

func (h *Handler) ValidateCertificate(c *fiber.Ctx) error {
	req := new(models.CertificateCheckRequest)
	if err := c.BodyParser(req); err != nil {
		h.Logger.Error(err.Error())
		return fiber.NewError(http.StatusBadRequest, "Invalid Request Body")
	}

	if strings.TrimSpace(req.PrivateKey) == "" ||
		strings.TrimSpace(req.NodeCertificate) == "" {
		h.Logger.Error("private_key, node_certificate can't be empty.")
		return fiber.NewError(http.StatusBadRequest, "private_key, node_certificate can't be empty, Please provide all the fields.")
	}

	res := h.ValidateCertificateService.CertificateValidation(*req)
	return c.JSON(response.BuildSuccessResponse(res))
}
