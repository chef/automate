package trigger

import "github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"

type NfsBackupConfigCheck struct{}

func NewNfsBackupConfigCheck() *NfsBackupConfigCheck {
	return &NfsBackupConfigCheck{}
}

func (ss *NfsBackupConfigCheck) Run(config models.Config) map[string]models.CheckTriggerResponse {
	m := map[string]models.CheckTriggerResponse{
		"f": models.CheckTriggerResponse{},
	}
	return m
}
