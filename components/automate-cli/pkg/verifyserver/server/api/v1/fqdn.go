package v1

import (
	"net/http"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/constants"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/response"
	"github.com/gofiber/fiber/v2"
)

func (h *Handler) CheckFqdn(c *fiber.Ctx) error {
	req := new(models.FqdnRequest)
	if err := c.BodyParser(req); err != nil {
		h.Logger.Error(err.Error())
		return fiber.NewError(http.StatusBadRequest, "Invalid Body Request")
	}

	if req.Fqdn == "" || req.RootCert == "" || len(req.Nodes) == 0 {
		h.Logger.Error("Fqdn, Root Cert and Nodes can't be empty.")
		return fiber.NewError(http.StatusBadRequest, "Fqdn, Root Cert and Nodes can't be empty, Please provide all the required fields.")
	}

	if req.IsAfterDeployment &&
		(req.NodeType == "" || (req.NodeType != constants.AUTOMATE && req.NodeType != constants.CHEF_INFRA_SERVER)) {
		h.Logger.Error("Node Type should be automate or chef-infra-server.")
		return fiber.NewError(http.StatusBadRequest, "Node Type should be automate or chef-infra-server, Please provide Node Type.")
	}

	res := h.FqdnService.CheckFqdnReachability(*req, constants.DEFAULT_HTTPS_PORT)
	return c.JSON(response.BuildSuccessResponse(res))
}
