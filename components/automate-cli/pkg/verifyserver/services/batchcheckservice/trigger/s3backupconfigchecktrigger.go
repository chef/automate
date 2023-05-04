package trigger

import "github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"

type S3BackupConfigCheck struct{}

// func (hrc *CheckTrigger) S3BackupConfigCheck(config models.Config) map[string]models.CheckTriggerResponse {
// 	return models.CheckTriggerResponse{}
// }

func (ss *S3BackupConfigCheck) Run(config models.Config) map[string]models.CheckTriggerResponse {
	m := map[string]models.CheckTriggerResponse{
        "f": models.CheckTriggerResponse{},
    }
	return m
}