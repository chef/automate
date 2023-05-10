package trigger

import "github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"

type SystemUserCheck struct{}

func NewSystemUserCheck() *SystemUserCheck {
	return &SystemUserCheck{}
}

func (suc *SystemUserCheck) Run(config models.Config) []models.CheckTriggerResponse {
	m := []models.CheckTriggerResponse{
		models.CheckTriggerResponse{},
	}
	return m
}
