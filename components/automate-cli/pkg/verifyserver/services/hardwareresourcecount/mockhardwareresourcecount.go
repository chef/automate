package hardwareresourcecount

import "github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"

type MockHardwareResourceCountService struct {
	GetHardwareResourceCountFunc func(models.HardwareResourceRequest) []models.HardwareResourceResponse
}

func (mhrc *MockHardwareResourceCountService) GetHardwareResourceCount(req models.HardwareResourceRequest) []models.HardwareResourceResponse {
	return mhrc.GetHardwareResourceCountFunc(req)
}
