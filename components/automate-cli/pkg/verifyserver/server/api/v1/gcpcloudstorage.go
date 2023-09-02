package v1

import (
	"fmt"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/response"
	"github.com/gofiber/fiber/v2"
)

func (h *Handler) GetGCPCloudStorageConfig(c *fiber.Ctx) error {
	var bucketAccess *models.Checks
	var checks []models.Checks
	gcpConfigRequest := new(models.GCPCloudStorageConfigRequest)
	if err := c.BodyParser(&gcpConfigRequest); err != nil {
		errString := fmt.Sprintf("GCP config request body parsing failed: %v", err.Error())
		h.Logger.Error(fmt.Errorf(errString))
		return fiber.NewError(fiber.StatusBadRequest, err.Error())
	}
	gcpConnection := h.GCPConfigService.GetGCPConnection(c.Context(), gcpConfigRequest)
	if gcpConnection.Passed {
		bucketAccess = h.GCPConfigService.GetBucketAccess(c.Context(), gcpConfigRequest)
		checks = append(checks, []models.Checks{*gcpConnection, *bucketAccess}...)
	} else {
		checks = append(checks, *gcpConnection)
	}

	return c.JSON(response.BuildSuccessResponse(&models.GCPCloudStorageResponse{
		Passed: gcpConnection.Passed && bucketAccess.Passed,
		Checks: checks,
	}))
}
