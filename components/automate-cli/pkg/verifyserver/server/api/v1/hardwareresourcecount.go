package v1

import (
	"net/http"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/response"
	"github.com/gofiber/fiber"
)

func (h *Handler) HardwareResourceCount(c *fiber.Ctx) {
	req := new(models.Hardware)
	if err := c.BodyParser(req); err != nil {
		h.Logger.Error(err.Error())
		c.Next(&fiber.Error{Code: http.StatusBadRequest, Message: "Invalid Body Request"})
		return
	}

	if req.AutomateNodeCount != len(req.AutomateNodeIps) ||
		req.ChefInfraServerNodeCount != len(req.ChefInfraServerNodeIps) ||
		req.PostgresqlNodeCount != len(req.PostgresqlNodeIps) ||
		req.OpenSearchNodeCount != len(req.OpenSearchNodeIps) {
		h.Logger.Error("Node Count and length of Node Ips should be equal.")
		c.Next(&fiber.Error{Code: http.StatusBadRequest, Message: "Node Count and length of Node Ips should be equal."})
		return
	}
	res := h.HardwareResourceCountService.GetHardwareResourceCount(*req)
	c.JSON(response.BuildSuccessResponse(res))
}
