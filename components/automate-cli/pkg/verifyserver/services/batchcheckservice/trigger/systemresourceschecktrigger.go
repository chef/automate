package trigger

import "github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"

type SystemResourceCheck struct{}

func NewSystemResourceCheck() *SystemResourceCheck {
	return &SystemResourceCheck{}
}

func (src *SystemResourceCheck) Run(config models.Config) []models.CheckTriggerResponse {
	m := []models.CheckTriggerResponse{
		models.CheckTriggerResponse{},
	}
	return m
}
