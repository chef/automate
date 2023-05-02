package v1

import (
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/gofiber/fiber"
)

func (h *Handler) BatchCheck(c *fiber.Ctx) {
	h.Logger.Info("Request received")
	req := new(models.BatchCheckRequest)

	if err := c.BodyParser(req); err != nil {
		h.Logger.Error(err.Error())
		c.Status(fiber.StatusBadRequest).JSON(err.Error())
	}
	h.BatchCheckService.BatchCheck(req.Checks, req.Config)
}
