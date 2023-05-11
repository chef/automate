package trigger

import "github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"

type NfsBackupConfigCheck struct{}

func NewNfsBackupConfigCheck() *NfsBackupConfigCheck {
	return &NfsBackupConfigCheck{}
}

func (nbc *NfsBackupConfigCheck) Run(config models.Config) []models.CheckTriggerResponse {
	m := []models.CheckTriggerResponse{
		models.CheckTriggerResponse{},
	}
	return m
}
