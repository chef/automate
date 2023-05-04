package trigger

import "github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"

type SoftwareVersionCheck struct{}

// func (hrc *CheckTrigger) SoftwareVersionCheck(config models.Config) map[string]models.CheckTriggerResponse {
// 	return models.CheckTriggerResponse{}
// }

func (ss *SoftwareVersionCheck) Run(config models.Config) map[string]models.CheckTriggerResponse {
	m := map[string]models.CheckTriggerResponse{
        "f": models.CheckTriggerResponse{},
    }
	return m
}