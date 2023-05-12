package v1

import (
	"fmt"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/gofiber/fiber"
)

func (h *Handler) CheckOSBackupS3(c *fiber.Ctx) {

	req := new(models.S3BackupDetails)

	if err := c.BodyParser(req); err != nil {
		errString := fmt.Sprintf("Invalid request for S3 backup check: %v", err.Error())
		h.Logger.Error(fmt.Errorf(errString))
		c.Status(fiber.StatusBadRequest).JSON(errString)
		return
	}

	fmt.Println("In handler got the req ", req)

	resp, err := h.OSBackupService.OSS3BackupVerify(*req, c)
	if err != nil {

	}

	c.Status(fiber.StatusOK).JSON(resp)

}
