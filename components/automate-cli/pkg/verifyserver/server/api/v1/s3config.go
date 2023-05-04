package v1

import (
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/response"
	"github.com/gofiber/fiber"
)

func (h *Handler) GetS3Config(c *fiber.Ctx) {
	s3ConfigRequest := new(models.S3ConfigRequest)
	if err := c.BodyParser(&s3ConfigRequest); err != nil {
		c.Status(503).Send(err)
		return
	}
	s3Connection := h.S3ConfigService.GetS3Connection(*s3ConfigRequest)
	bucketAccess := h.S3ConfigService.GetBucketAccess(*s3ConfigRequest)
	c.JSON(response.BuildSuccessResponse(&models.S3ConfigResponse{
		Passed: true,
		Checks: []models.ServiceCheck{
			s3Connection, bucketAccess,
		},
	}))
}
