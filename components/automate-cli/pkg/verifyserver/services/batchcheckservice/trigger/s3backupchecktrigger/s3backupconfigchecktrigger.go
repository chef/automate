package s3backupchecktrigger

import (
	"net/http"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/constants"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/services/batchcheckservice/trigger"
	"github.com/chef/automate/lib/logger"
)

type S3BackupConfigCheck struct {
	log  logger.Logger
	port string
}

func NewS3BackupConfigCheck(log logger.Logger, port string) *S3BackupConfigCheck {
	return &S3BackupConfigCheck{
		log:  log,
		port: port,
	}
}

func (svc *S3BackupConfigCheck) Run(config models.Config) []models.CheckTriggerResponse {
	return trigger.RunCheckOnSpecifiedNode(config.Hardware.AutomateNodeIps, svc.log, svc.port, constants.S3_BACKUP_CHECK_API_PATH, constants.AUTOMATE, http.MethodPost, config.Backup.ObjectStorage)
}
