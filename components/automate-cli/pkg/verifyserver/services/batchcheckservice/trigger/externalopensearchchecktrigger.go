package trigger

import "github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"

type ExternalOpensearchCheck struct{}

// func (hrc *CheckTrigger) ExternalOpensearchCheck(config models.Config) map[string]models.CheckTriggerResponse {
// 	return models.CheckTriggerResponse{}
// }

func (ss *ExternalOpensearchCheck) Run(config models.Config) map[string]models.CheckTriggerResponse {
	m := map[string]models.CheckTriggerResponse{
        "f": models.CheckTriggerResponse{},
    }
	return m
}