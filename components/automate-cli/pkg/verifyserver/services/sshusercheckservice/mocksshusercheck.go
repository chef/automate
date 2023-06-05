package sshusercheckservice

import "github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"

type MockSshUserCheckService struct {
	CheckSshUserDetailsFunc func(*models.SshUserChecksRequest) (*models.SshUserChecksResponse, error)
}

func (mssu *MockSshUserCheckService) CheckSshUserDetails(req *models.SshUserChecksRequest) (*models.SshUserChecksResponse, error) {
	return mssu.CheckSshUserDetailsFunc(req)
}
