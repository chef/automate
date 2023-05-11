package trigger

import "github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"

type FqdnCheck struct{}

func NewFqdnCheck() *FqdnCheck {
	return &FqdnCheck{}
}

func (fqc *FqdnCheck) Run(config models.Config) []models.CheckTriggerResponse {
	m := []models.CheckTriggerResponse{
		models.CheckTriggerResponse{},
	}
	return m
}
