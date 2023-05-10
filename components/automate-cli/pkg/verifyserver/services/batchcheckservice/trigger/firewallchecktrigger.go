package trigger

import "github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"

type FirewallCheck struct{}

func NewFirewallCheck() *FirewallCheck {
	return &FirewallCheck{}
}

func (fc *FirewallCheck) Run(config models.Config) []models.CheckTriggerResponse {
	m := []models.CheckTriggerResponse{
		models.CheckTriggerResponse{},
	}
	return m
}
