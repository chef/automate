package trigger

import "github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"

type SoftwareVersionCheck struct{}

func NewSoftwareVersionCheck() *SoftwareVersionCheck {
	return &SoftwareVersionCheck{}
}

func (svc *SoftwareVersionCheck) Run(config models.Config) []models.CheckTriggerResponse {
	m := []models.CheckTriggerResponse{
		models.CheckTriggerResponse{},
	}
	return m
}
