package trigger

import "github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"

type CertificateCheck struct {
}

// func (hrc *CertificateCheck) CertificateCheck(config models.Config) map[string]models.CheckTriggerResponse {
// 	return models.CheckTriggerResponse{}
// }

func (ss *CertificateCheck) Run(config models.Config) map[string]models.CheckTriggerResponse {
	m := map[string]models.CheckTriggerResponse{
        "f": models.CheckTriggerResponse{},
    }
	return m
}
