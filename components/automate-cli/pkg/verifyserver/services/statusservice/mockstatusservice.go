package statusservice

import "github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"

type MockStatusService struct {
	GetServicesFunc func() []models.ServiceDetails
}

func (fsu *MockStatusService) GetServices() []models.ServiceDetails {
	return fsu.GetServicesFunc()
}
