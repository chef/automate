package systemresourceservice

import "github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"

type MockSystemResourcesService struct {
	GetSystemResourcesForDeploymentFunc func(string, string) (*models.ApiResult, error)
}

func (msrs *MockSystemResourcesService) GetSystemResourcesForDeployment(nodeType, deploymentState string) (*models.ApiResult, error) {
	return msrs.GetSystemResourcesForDeploymentFunc(nodeType, deploymentState)
}
