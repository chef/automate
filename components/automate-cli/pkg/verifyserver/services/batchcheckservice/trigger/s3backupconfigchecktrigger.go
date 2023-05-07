package trigger

import "github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"

type S3BackupConfigCheck struct{}

func NewS3BackupConfigCheck() *S3BackupConfigCheck {
	return &S3BackupConfigCheck{}
}

func (ss *S3BackupConfigCheck) Run(config models.Config) map[string]models.CheckTriggerResponse {
	m := map[string]models.CheckTriggerResponse{
		"f": models.CheckTriggerResponse{},
	}
	return m
}
