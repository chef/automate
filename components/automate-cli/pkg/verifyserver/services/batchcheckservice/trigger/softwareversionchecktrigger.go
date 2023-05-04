package trigger

import "github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"

type SoftwareVersionCheck struct{}

func NewSoftwareVersionCheck() *SoftwareVersionCheck {
	return &SoftwareVersionCheck{}
}

func (ss *SoftwareVersionCheck) Run(config models.Config) map[string]models.CheckTriggerResponse {
	m := map[string]models.CheckTriggerResponse{
		"f": models.CheckTriggerResponse{},
	}
	return m
}
