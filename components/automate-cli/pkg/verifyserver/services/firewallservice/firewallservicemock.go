package firewallservice

import (
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
)

type MockFirewallService struct {
	GetFirewallDetailsFunc func(reqBody models.FirewallRequest) models.FirewallResponse
}

func (mnm *MockFirewallService) GetFirewallDetails(reqBody models.FirewallRequest) models.FirewallResponse {
	return mnm.GetFirewallDetailsFunc(reqBody)
}
