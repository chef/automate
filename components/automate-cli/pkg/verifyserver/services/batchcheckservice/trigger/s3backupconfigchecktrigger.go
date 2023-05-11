package trigger

import "github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"

type S3BackupConfigCheck struct{}

func NewS3BackupConfigCheck() *S3BackupConfigCheck {
	return &S3BackupConfigCheck{}
}

func (sbc *S3BackupConfigCheck) Run(config models.Config) []models.CheckTriggerResponse {
	m := []models.CheckTriggerResponse{
		models.CheckTriggerResponse{},
	}
	return m
}
