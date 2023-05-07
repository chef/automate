package trigger

import "github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"

type FqdnCheck struct{}

func NewFqdnCheck() *FqdnCheck {
	return &FqdnCheck{}
}

func (ss *FqdnCheck) Run(config models.Config) map[string]models.CheckTriggerResponse {
	m := map[string]models.CheckTriggerResponse{
		"f": models.CheckTriggerResponse{},
	}
	return m
}
