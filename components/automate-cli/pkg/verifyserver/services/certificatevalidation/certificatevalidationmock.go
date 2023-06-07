package certificatevalidation

import "github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"

type MockCertificateValidationService struct {
	CertificateValidationFunc func(models.CertificateCheckRequest) models.CertificateCheckResponse
}

func (mvc *MockCertificateValidationService) CertificateValidation(req models.CertificateCheckRequest) models.CertificateCheckResponse {
	return mvc.CertificateValidationFunc(req)
}
