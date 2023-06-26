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
	res := h.HardwareResourceCountService.GetHardwareResourceCount(*req)
	return c.JSON(response.BuildSuccessResponse(res))
}
