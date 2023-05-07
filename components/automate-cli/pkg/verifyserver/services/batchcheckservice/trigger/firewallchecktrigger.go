package trigger

import "github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"

type FirewallCheck struct{}

func NewFirewallCheck() *FirewallCheck {
	return &FirewallCheck{}
}

func (ss *FirewallCheck) Run(config models.Config) map[string]models.CheckTriggerResponse {
	m := map[string]models.CheckTriggerResponse{
		"f": models.CheckTriggerResponse{},
	}
	return m
}
