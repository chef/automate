package trigger

import "github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"

type ExternalPostgresCheck struct{}

func NewExternalPostgresCheck() *ExternalPostgresCheck {
	return &ExternalPostgresCheck{}
}

func (ss *ExternalPostgresCheck) Run(config models.Config) map[string]models.CheckTriggerResponse {
	m := map[string]models.CheckTriggerResponse{
		"f": models.CheckTriggerResponse{},
	}
	return m
}
