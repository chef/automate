package v1

import (
	"net/http"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/response"
	"github.com/gofiber/fiber"
)

func (h *Handler) HardwareResourceCount(c *fiber.Ctx) {
	req := new(models.HardwareResourceRequest)
	if err := c.BodyParser(req); err != nil {
		c.Next(&fiber.Error{Code: http.StatusBadRequest, Message: "Invalid Body Request"})
		return
	}

	if req.AutomateNodeCount != len(req.AutomateNodeIps) || req.ChefInfraServerNodeCount != len(req.ChefInfraServerNodeIps) || req.PostgresqlNodeCount != len(req.PostgresqlNodeIps) || req.OpenSearchNodeCount != len(req.OpenSearchNodeIps) {
		c.Next(&fiber.Error{Code: http.StatusBadRequest, Message: "Mismatch of Node Count and length of Node Ips."})
		return
	}

	res := h.HardwareResourceCountService.GetHardwareResourceCount(*req)
	c.JSON(response.BuildSuccessResponse(res))
}
