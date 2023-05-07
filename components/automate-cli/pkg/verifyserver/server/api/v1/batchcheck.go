package v1

import (
	"errors"
	"fmt"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/constants"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/lib/stringutils"
	"github.com/gofiber/fiber"
)

func (h *Handler) BatchCheck(c *fiber.Ctx) {
	req := new(models.BatchCheckRequest)
	if err := c.BodyParser(req); err != nil {
		errString := fmt.Sprintf("batch check request body parsing failed: %v", err.Error())
		h.Logger.Error(fmt.Errorf(errString))
		c.Status(fiber.StatusBadRequest).JSON(errString)
		return
	}
	err := validateChecks(req.Checks)
	if err != nil {
		c.Status(fiber.StatusBadRequest).JSON(err.Error())
		return
	}

	resp := h.BatchCheckService.BatchCheck(req.Checks, req.Config)
	c.Status(fiber.StatusOK).JSON(resp)
}

func validateChecks(checks []string) error {
	if len(checks) == 0 {
		err := errors.New("check cannot be empty")
		return err
	}
	allChecks := append(constants.GetBastionChecks(), constants.GetRemoteChecks()...)
	diff := stringutils.SliceDifference(checks, allChecks)
	if len(diff) > 0 {
		err := fmt.Errorf("following checks are not supported %v", diff)
		return err
	}
	return nil
}
