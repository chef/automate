package v1

import (
	"errors"
	"fmt"
	"net/http"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/constants"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/response"
	"github.com/chef/automate/lib/stringutils"
	"github.com/gofiber/fiber/v2"
)

func (h *Handler) BatchCheck(c *fiber.Ctx) error {
	req := new(models.BatchCheckRequest)
	if err := c.BodyParser(req); err != nil {
		errString := fmt.Sprintf("batch check request body parsing failed: %v", err.Error())
		h.Logger.Error(fmt.Errorf(errString))
		return fiber.NewError(http.StatusBadRequest, errString)
	}
	err := validateChecks(req.Checks)
	if err != nil {
		return fiber.NewError(http.StatusBadRequest, err.Error())
	}

	resp, err := h.BatchCheckService.BatchCheck(req.Checks, req.Config)
	if err != nil {
		return fiber.NewError(http.StatusInternalServerError, err.Error())
	}
	return c.Status(fiber.StatusOK).JSON(response.BuildSuccessResponse(resp.Result))
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
