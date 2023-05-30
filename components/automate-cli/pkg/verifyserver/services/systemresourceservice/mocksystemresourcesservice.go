package systemresourceservice

import (
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/enums"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
)

type MockSystemResourcesServiceImpl struct {
	GetSystemResourcesForDeploymentFunc func(enums.NodeType, enums.DeploymentState) *models.ApiResult
}

func (msrs *MockSystemResourcesServiceImpl) GetSystemResourcesForDeployment(nodeType enums.NodeType, deploymentState enums.DeploymentState) *models.ApiResult {
	return msrs.GetSystemResourcesForDeploymentFunc(nodeType, deploymentState)
}
