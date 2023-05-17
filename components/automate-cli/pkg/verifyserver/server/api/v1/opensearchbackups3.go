package v1

import (
	"fmt"
	"net/http"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/response"
	"github.com/gofiber/fiber"
)

func (h *Handler) CheckOSBackupS3(c *fiber.Ctx) {

	req := new(models.S3BackupDetails)

	if err := c.BodyParser(req); err != nil {
		errString := fmt.Sprintf("Invalid request for S3 backup check: %v", err.Error())
		h.Logger.Error(fmt.Errorf(errString))
		c.Next(&fiber.Error{Code: http.StatusBadRequest, Message: err.Error()})
		return
	}

	if !requestValidator(*req) {
		errString := fmt.Sprintf("Invalid request body for S3 backup check")
		h.Logger.Error(fmt.Errorf(errString))
		c.Next(&fiber.Error{Code: http.StatusBadRequest, Message: errString})
		return
	}

	resp, err := h.OSBackupService.OSS3BackupVerify(*req, c)
	if err != nil {
		errString := fmt.Sprintf("S3 backup check failed : %v", err.Error())
		h.Logger.Error(fmt.Errorf(errString))
		c.Next(&fiber.Error{Code: http.StatusInternalServerError, Message: err.Error()})
	}

	c.JSON(response.BuildSuccessResponse(resp))

}

func requestValidator(req models.S3BackupDetails) bool {

	if len(req.Endpoint) == 0 || len(req.Username) == 0 || len(req.Password) == 0 || len(req.S3Bucket) == 0 || len(req.S3BasePath) == 0 ||
		len(req.AccessKey) == 0 || len(req.SecretKey) == 0 || len(req.AWSRegion) == 0 || len(req.AWSRoleArn) == 0 {
		return false
	}
	return true
}
