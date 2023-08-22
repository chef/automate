package v1

import (
	"net"
	"net/http"
	"strings"
	"time"

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

	if strings.TrimSpace(req.Fqdn) == "" || len(req.Nodes) == 0 {
		h.Logger.Error("fqdn, nodes can't be empty.")
		return fiber.NewError(http.StatusBadRequest, "fqdn, nodes can't be empty, Please provide all the required fields.")
	}

	for _, ip := range req.Nodes {
		if net.ParseIP(ip) == nil {
			h.Logger.Errorf("%v node IP is not valid", ip)
			return fiber.NewError(http.StatusBadRequest, "Node IP is not valid, Please provide the valid format IP.")
		}
	}

	if req.IsAfterDeployment &&
		(req.NodeType == "" || (req.NodeType != constants.AUTOMATE && req.NodeType != constants.CHEF_INFRA_SERVER)) {
		h.Logger.Error("node_type should be automate or chef-infra-server.")
		return fiber.NewError(http.StatusBadRequest, "node_type should be automate or chef-infra-server, Please provide node_type.")
	}

	res := h.FqdnService.CheckFqdnReachability(*req, constants.DEFAULT_HTTPS_PORT, time.Minute*1)
	return c.JSON(response.BuildSuccessResponse(res))
}
