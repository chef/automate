package softwareversionservice

import (
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
)


type MockSoftwareVersionService struct {
	GetSoftwareServicesFunc func() (models.SoftwareVersionDetails, error)
}

func (msv *MockSoftwareVersionService) GetSoftwareVersionServices() (models.SoftwareVersionDetails, error) {
	return msv.GetSoftwareServicesFunc()
}


