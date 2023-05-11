package trigger

import "github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"

type ExternalOpensearchCheck struct{}

func NewExternalOpensearchCheck() *ExternalOpensearchCheck {
	return &ExternalOpensearchCheck{}
}

func (eosc *ExternalOpensearchCheck) Run(config models.Config) []models.CheckTriggerResponse {
	m := []models.CheckTriggerResponse{
		models.CheckTriggerResponse{},
	}
	return m
}
