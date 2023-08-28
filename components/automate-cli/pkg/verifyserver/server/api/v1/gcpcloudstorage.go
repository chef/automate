package v1

import (
	"fmt"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/constants"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/response"
	"github.com/gofiber/fiber/v2"
)

func (h *Handler) GetGCPCloudStorageConfig(c *fiber.Ctx) error {
	var bucketAccess *models.Checks
	gcpConfigRequest := new(models.GCPCloudStorageConfigRequest)
	if err := c.BodyParser(&gcpConfigRequest); err != nil {
		errString := fmt.Sprintf("GCP config request body parsing failed: %v", err.Error())
		h.Logger.Error(fmt.Errorf(errString))
		return fiber.NewError(fiber.StatusBadRequest, err.Error())
	}
	fmt.Printf("gcpConfigRequest: %+v\n", gcpConfigRequest)
	gcpConnection := h.GCPConfigService.GetGCPConnection(gcpConfigRequest)
	if gcpConnection.Passed {
		bucketAccess = h.GCPConfigService.GetBucketAccess(gcpConfigRequest)
	} else {
		bucketAccess = &models.Checks{
			Title:         constants.GCP_BUCKET_ACCESS_TITLE,
			Passed:        false,
			SuccessMsg:    "",
			ErrorMsg:      constants.GCP_BUCKET_ACCESS_ERROR_MSG,
			ResolutionMsg: constants.GCP_BUCKET_ACCESS_RESOLUTION_MSG,
		}
	}

	return c.JSON(response.BuildSuccessResponse(&models.GCPCloudStorageResponse{
		Passed: gcpConnection.Passed && bucketAccess.Passed,
		Checks: []models.Checks{
			*gcpConnection, *bucketAccess,
		},
	}))
}
