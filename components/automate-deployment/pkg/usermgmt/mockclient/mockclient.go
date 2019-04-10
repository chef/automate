package mockclient

import (
	"context"

	"github.com/chef/automate/components/automate-deployment/pkg/usermgmt"
)

type mockUserMgmtClient struct {
	createUserResp       string
	createUserWasCreated bool
	createUserErr        error
	addUserErr           error
}

// NewMockUserMgmtClient mocks the usermgmt.UserMgmt interface and returns the values and errors
// for the methods in the interface based on the values passed in.
func NewMockUserMgmtClient(createUserResp string,
	createUserWasCreated bool, createUserErr error, addUserErr error) usermgmt.UserMgmt {

	return &mockUserMgmtClient{
		createUserResp:       createUserResp,
		createUserWasCreated: createUserWasCreated,
		createUserErr:        createUserErr,
		addUserErr:           addUserErr,
	}
}

// CreateUser mocks the call to the real CreateUser.
func (m *mockUserMgmtClient) CreateUser(ctx context.Context,
	name, email, password string) (userID string, wasCreated bool, err error) {

	return m.createUserResp, m.createUserWasCreated, m.createUserErr
}

// AddUserToAdminTeam mocks the call to the real AddUserToAdminTeam.
func (m *mockUserMgmtClient) AddUserToAdminTeam(ctx context.Context, userID string) error {
	return m.addUserErr
}
