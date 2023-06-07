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

	if strings.TrimSpace(req.RootCertificate) == "" ||
		strings.TrimSpace(req.PrivateKey) == "" ||
		strings.TrimSpace(req.NodeCertificate) == "" ||
		strings.TrimSpace(req.AdminPrivateKey) == "" ||
		strings.TrimSpace(req.AdminCertificate) == "" {
		h.Logger.Error("root_certificate, private_key, node_certificate, admin_private_key and admin_certificate can't be empty.")
		return fiber.NewError(http.StatusBadRequest, "root_certificate, private_key, node_certificate, admin_private_key and admin_certificate can't be empty, Please provide all the fields.")
	}

	res := h.ValidateCertificateService.CertificateValidation(*req)
	return c.JSON(response.BuildSuccessResponse(res))
}
