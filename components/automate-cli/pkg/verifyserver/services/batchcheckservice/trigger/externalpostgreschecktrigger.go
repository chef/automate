package trigger

import "github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"

type ExternalPostgresCheck struct{}

// func (hrc *CheckTrigger) ExternalPostgresCheck(config models.Config) map[string]models.CheckTriggerResponse {
// 	return models.CheckTriggerResponse{}
// }

func (ss *ExternalPostgresCheck) Run(config models.Config) map[string]models.CheckTriggerResponse {
	m := map[string]models.CheckTriggerResponse{
        "f": models.CheckTriggerResponse{},
    }
	return m
}