package trigger

import "github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"

type CertificateCheck struct {
}

func NewCertificateCheck() *CertificateCheck {
	return &CertificateCheck{}
}

func (ss *CertificateCheck) Run(config models.Config) map[string]models.CheckTriggerResponse {
	m := map[string]models.CheckTriggerResponse{
		"f": models.CheckTriggerResponse{},
	}
	return m
}
