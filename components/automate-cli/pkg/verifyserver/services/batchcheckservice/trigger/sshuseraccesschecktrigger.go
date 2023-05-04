package trigger

import "github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"

type SshUserAccessCheck struct {
}

func NewSshUserAccessCheck() *SshUserAccessCheck {
	return &SshUserAccessCheck{}
}

func (ss *SshUserAccessCheck) Run(config models.Config) map[string]models.CheckTriggerResponse {
	m := map[string]models.CheckTriggerResponse{}
	return m
}
