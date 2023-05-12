package softwareversionservice

import (
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
)

type MockSoftwareVersionService struct {
	GetSoftwareServicesFunc func(string) (*models.SoftwareVersionDetails, error)
}

func (msv *MockSoftwareVersionService) GetSoftwareVersionServices(string) (*models.SoftwareVersionDetails, error) {
	return msv.GetSoftwareServicesFunc("opensearch")
}
