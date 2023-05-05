package v1

import (
	"errors"
	"fmt"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/constants"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/response"
	"github.com/ghetzel/go-stockutil/sliceutil"
	"github.com/gofiber/fiber"
)

func (h *Handler) BatchCheck(c *fiber.Ctx) {
	req := new(models.BatchCheckRequest)
	if err := c.BodyParser(req); err != nil {
		h.Logger.Error(fmt.Errorf("batch check request body parsing failed %v", err.Error()))
		c.JSON(response.BuildFailedResponse(fiber.NewError(fiber.StatusBadRequest, err.Error())))
	}
	err := validateChecks(req.Checks)
	if err != nil {
		c.JSON(response.BuildFailedResponse(fiber.NewError(fiber.StatusBadRequest, err.Error())))
	}

	resp := h.BatchCheckService.BatchCheck(req.Checks, req.Config)
	c.Status(fiber.StatusOK).JSON(resp)
}

func validateChecks(checks []string) error {
	if len(checks) == 0 {
		err := errors.New("check cannot be empty")
		return err
	}
	allChecks := constants.GetAllChecks()
	diff := sliceutil.Difference(checks, allChecks)
	if len(diff) > 0 {
		err := fmt.Errorf("following checks are not supported %v", diff)
		return err
	}
	return nil
}
