package v1

import (
	"net/http"
	"strings"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/constants"
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

	if strings.TrimSpace(req.NodeType) == "" ||
		(req.NodeType != constants.AUTOMATE &&
			req.NodeType != constants.CHEF_INFRA_SERVER &&
			req.NodeType != constants.OPENSEARCH &&
			req.NodeType != constants.POSTGRESQL) {
		h.Logger.Error("node_type should be automate, chef-infra-server, opensearch or postgresql.")
		return fiber.NewError(http.StatusBadRequest, "node_type should be automate, chef-infra-server, opensearch or postgresql. Please provide node_type.")
	}

	if strings.TrimSpace(req.RootCertificate) == "" ||
		strings.TrimSpace(req.PrivateKey) == "" ||
		strings.TrimSpace(req.NodeCertificate) == "" {
		h.Logger.Error("root_certificate, private_key, node_certificate can't be empty.")
		return fiber.NewError(http.StatusBadRequest, "root_certificate, private_key, node_certificate can't be empty, Please provide all the fields.")
	}

	if req.NodeType == constants.OPENSEARCH &&
		(strings.TrimSpace(req.AdminPrivateKey) == "" || strings.TrimSpace(req.AdminCertificate) == "") {
		h.Logger.Error("admin_private_key and admin_certificate can't be empty.")
		return fiber.NewError(http.StatusBadRequest, "admin_private_key and admin_certificate can't be empty. Please provide all the fields.")
	}

	res := h.ValidateCertificateService.CertificateValidation(*req)
	return c.JSON(response.BuildSuccessResponse(res))
}
