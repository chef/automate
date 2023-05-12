package softwareversionservice

import (
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
)

type MockSoftwareVersionService struct {
	GetSoftwareDetailsFunc func(string) (*models.SoftwareVersionDetails, error)
}

func (msv *MockSoftwareVersionService) GetSoftwareVersionDetails(string) (*models.SoftwareVersionDetails, error) {
	return msv.GetSoftwareDetailsFunc("opensearch")
}