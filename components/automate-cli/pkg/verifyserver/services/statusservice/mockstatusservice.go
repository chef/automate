package statusservice

import "github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"

type MockStatusService struct {
	GetServicesFunc func() (*[]models.ServiceDetails, error)
}

func (mss *MockStatusService) GetServices() (*[]models.ServiceDetails, error) {
	return mss.GetServicesFunc()
}
