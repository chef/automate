package systemuserservice

import "github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"

type MockSystemUserService struct {
	GetSystemUserServiceDetailsFunc func() *models.SystemUserResponse
}

// GetSystemUserServiceDetails implements SystemUser
func (msu *MockSystemUserService) GetSystemUserServiceDetails() *models.SystemUserResponse {
	return msu.GetSystemUserServiceDetailsFunc()
}

