package portreachableservice

import (
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
)

type MockPortReachableService struct {
	GetPortReachableDetailsFunc func(reqBody models.PortReachableRequest) models.Checks
}

func (mpr *MockPortReachableService) GetPortReachableDetails(reqBody models.PortReachableRequest) models.Checks {
	return mpr.GetPortReachableDetailsFunc(reqBody)
}
