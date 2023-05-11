package trigger

import "github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"

type CertificateCheck struct {
}

func NewCertificateCheck() *CertificateCheck {
	return &CertificateCheck{}
}

func (cc *CertificateCheck) Run(config models.Config) []models.CheckTriggerResponse {
	m := []models.CheckTriggerResponse{
		models.CheckTriggerResponse{},
	}
	return m
}
