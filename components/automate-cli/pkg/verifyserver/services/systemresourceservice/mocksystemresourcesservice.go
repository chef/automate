package systemresourceservice

import (
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/constants"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
)

type MockSystemResourcesServiceImpl struct {
	GetSystemResourcesForDeploymentFunc func(constants.NodeType, constants.DeploymentState) *models.ApiResult
}

func (msrs *MockSystemResourcesServiceImpl) GetSystemResourcesForDeployment(nodeType constants.NodeType, deploymentState constants.DeploymentState) *models.ApiResult {
	return msrs.GetSystemResourcesForDeploymentFunc(nodeType, deploymentState)
}
