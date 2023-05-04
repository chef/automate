package trigger

import "github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"

type SystemResourceCheck struct{}

func NewSystemResourceCheck() *SystemResourceCheck {
	return &SystemResourceCheck{}
}

func (ss *SystemResourceCheck) Run(config models.Config) map[string]models.CheckTriggerResponse {
	m := map[string]models.CheckTriggerResponse{
		"f": models.CheckTriggerResponse{},
	}
	return m
}
