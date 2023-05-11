package trigger

import "github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"

type ExternalPostgresCheck struct{}

func NewExternalPostgresCheck() *ExternalPostgresCheck {
	return &ExternalPostgresCheck{}
}

func (epc *ExternalPostgresCheck) Run(config models.Config) []models.CheckTriggerResponse {
	m := []models.CheckTriggerResponse{
		models.CheckTriggerResponse{},
	}
	return m
}
