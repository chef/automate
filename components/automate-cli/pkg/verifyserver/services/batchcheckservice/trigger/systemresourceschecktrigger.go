package trigger

import "github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"

type SystemResourceCheck struct{}

// func (hrc *CheckTrigger) SystemResourceCheck(config models.Config) map[string]models.CheckTriggerResponse {
// 	return models.CheckTriggerResponse{}
// }

func (ss *SystemResourceCheck) Run(config models.Config) map[string]models.CheckTriggerResponse {
	m := map[string]models.CheckTriggerResponse{
        "f": models.CheckTriggerResponse{},
    }
	return m
}