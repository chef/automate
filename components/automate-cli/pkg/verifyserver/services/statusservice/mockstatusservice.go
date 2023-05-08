package statusservice

import "github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"

type MockStatusService struct {
	GetServicesFunc                   func() ([]models.ServiceDetails, error)
	GetServicesFromAutomateStatusFunc func() (map[string]models.ServiceDetails, error)
	GetServicesFromHabSvcStatusFunc   func() ([]models.ServiceDetails, error)
	ParseChefAutomateStatusFunc       func(output string) (map[string]models.ServiceDetails, error)
	ParseHabSvcStatusFunc             func(output string) ([]models.ServiceDetails, error)
	CheckIfBENodeFunc                 func(output string) bool
}

func (mss *MockStatusService) GetServices() ([]models.ServiceDetails, error) {
	return mss.GetServicesFunc()
}

func (mss *MockStatusService) GetServicesFromAutomateStatus() (map[string]models.ServiceDetails, error) {
	return mss.GetServicesFromAutomateStatusFunc()
}

func (mss *MockStatusService) GetServicesFromHabSvcStatus() ([]models.ServiceDetails, error) {
	return mss.GetServicesFromHabSvcStatusFunc()
}

func (mss *MockStatusService) ParseChefAutomateStatus(output string) (map[string]models.ServiceDetails, error) {
	return mss.ParseChefAutomateStatusFunc(output)
}

func (mss *MockStatusService) ParseHabSvcStatus(output string) ([]models.ServiceDetails, error) {
	return mss.ParseHabSvcStatusFunc(output)
}

func (mss *MockStatusService) CheckIfBENode(output string) bool {
	return mss.CheckIfBENodeFunc(output)
}
