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
		c.Next(&fiber.Error{Code: http.StatusBadRequest, Message: "Fqdn, Root Cert and Nodes can't be empty, Please provide all the required fields."})
		return
	}

	if req.IsAfterDeployment &&
		(req.NodeType == "" || (req.NodeType != constants.AUTOMATE && req.NodeType != constants.CHEF_INFRA_SERVER)) {
		h.Logger.Error("Node Type should be automate or chef-infra-server.")
		c.Next(&fiber.Error{Code: http.StatusBadRequest, Message: "Node Type should be automate or chef-infra-server, Please provide Node Type."})
		return
	}

	if req.IsAfterDeployment && req.ApiToken == "" {
		h.Logger.Error("Api Token can't be empty.")
		c.Next(&fiber.Error{Code: http.StatusBadRequest, Message: "Api Token can't be empty, it is needed for checking the Automate status."})
		return
	}

	res := h.FqdnService.CheckFqdnReachability(*req)
	return c.JSON(response.BuildSuccessResponse(res))
}
