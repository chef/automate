package v1

import (
	"net/http"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/response"
	"github.com/gofiber/fiber/v2"
)

func (h *Handler) HardwareResourceCount(c *fiber.Ctx) error {
	req := new(models.Hardware)
	if err := c.BodyParser(req); err != nil {
		h.Logger.Error(err.Error())
		return fiber.NewError(http.StatusBadRequest, "Invalid Body Request")
	}

	if req.AutomateNodeCount != len(req.AutomateNodeIps) ||
		req.ChefInfraServerNodeCount != len(req.ChefInfraServerNodeIps) ||
		req.PostgresqlNodeCount != len(req.PostgresqlNodeIps) ||
		req.OpenSearchNodeCount != len(req.OpenSearchNodeIps) {
		h.Logger.Error("Node Count and length of Node Ips should be equal.")
		return fiber.NewError(http.StatusBadRequest, "Node Count and length of Node Ips should be equal.")
	}
	res := h.HardwareResourceCountService.GetHardwareResourceCount(*req)
	return c.JSON(response.BuildSuccessResponse(res))
}
