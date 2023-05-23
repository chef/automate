package v1

import (
	"fmt"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/constants"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/response"
	"github.com/gofiber/fiber"
)

func (h *Handler) GetS3Config(c *fiber.Ctx) {
	var bucketAccess *models.Checks
	s3ConfigRequest := new(models.S3ConfigRequest)
	if err := c.BodyParser(&s3ConfigRequest); err != nil {
		errString := fmt.Sprintf("s3 config request body parsing failed: %v", err.Error())
		h.Logger.Error(fmt.Errorf(errString))
		c.Next(&fiber.Error{Code: fiber.StatusBadRequest, Message: err.Error()})
		return
	}
	s3Connection := h.S3ConfigService.GetS3Connection(s3ConfigRequest)
	if s3Connection.Passed {
		bucketAccess = h.S3ConfigService.GetBucketAccess(s3ConfigRequest)
	} else {
		bucketAccess = &models.Checks{
			Title:         constants.S3_BUCKET_ACCESS_TITLE,
			Passed:        false,
			SuccessMsg:    "",
			ErrorMsg:      constants.S3_BUCKET_ACCESS_ERROR_MSG,
			ResolutionMsg: constants.S3_BUCKET_ACCESS_RESOLUTION_MSG,
		}
	}
	c.JSON(response.BuildSuccessResponse(&models.S3ConfigResponse{
		Passed: s3Connection.Passed && bucketAccess.Passed,
		Checks: []models.Checks{
			*s3Connection, *bucketAccess,
		},
	}))
}
