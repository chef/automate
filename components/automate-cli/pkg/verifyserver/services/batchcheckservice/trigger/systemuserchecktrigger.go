package trigger

import "github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"

type SystemUserCheck struct{}

func NewSystemUserCheck() *SystemUserCheck {
	return &SystemUserCheck{}
}

func (ss *SystemUserCheck) Run(config models.Config) map[string]models.CheckTriggerResponse {
	m := map[string]models.CheckTriggerResponse{
		"f": models.CheckTriggerResponse{},
	}
	return m
}
