package client

import (
	"context"

	"google.golang.org/grpc"
	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/status"

	"github.com/chef/automate/api/interservice/local_user"
	teams "github.com/chef/automate/api/interservice/teams/v2"
	"github.com/chef/automate/components/automate-deployment/pkg/usermgmt"
	teams_storage "github.com/chef/automate/components/teams-service/storage"
	"github.com/chef/automate/lib/grpc/secureconn"
)

type userMgmtClient struct {
	teamsClient      teams.TeamsV2Client
	localUsersClient local_user.UsersMgmtClient
}

// NewUserMgmtClient returns a client capable of creating a new user
// and adding that user to the admins team.
func NewUserMgmtClient(ctx context.Context, factory *secureconn.Factory,
	localUserGRPCAddress, teamsGRPCAddress string) (usermgmt.UserMgmt, error) {

	usersConnection, err := factory.DialContext(
		ctx,
		"local-user-service",
		localUserGRPCAddress,
		grpc.WithBlock(),
	)
	if err != nil {
		return nil, err
	}
	localUsersClient := local_user.NewUsersMgmtClient(usersConnection)

	teamsConnection, err := factory.DialContext(
		ctx,
		"teams-service",
		teamsGRPCAddress,
		grpc.WithBlock(),
	)
	if err != nil {
		return nil, err
	}
	teamsClient := teams.NewTeamsV2Client(teamsConnection)

	return &userMgmtClient{
		teamsClient:      teamsClient,
		localUsersClient: localUsersClient,
	}, nil
}

// CreateUser either creates a new user or retrieves the ID or an error. It also
// returns true if the user was created and false if the user already existed.
func (u *userMgmtClient) CreateUser(ctx context.Context,
	name, email, password string) (userID string, wasCreated bool, err error) {

	response, err := u.localUsersClient.CreateUser(ctx, &local_user.CreateUserReq{
		Name:     name,
		Email:    email,
		Password: password,
	})

	if err != nil {
		if isAlreadyExists(err) {
			userResp, getErr := u.localUsersClient.GetUser(ctx, &local_user.Email{
				Email: email})
			if getErr != nil {
				return "", false, getErr
			}
			return userResp.Id, false, nil
		}
		return "", false, err
	}
	return response.Id, true, nil
}

func (u *userMgmtClient) AddUserToAdminTeam(ctx context.Context, userID string) error {
	adminsTeam, err := u.teamsClient.GetTeam(ctx,
		// (tc) By convention, this is the admins team name string and will properly be
		// updated here should that change in teams-service.
		&teams.GetTeamReq{Id: teams_storage.AdminsTeamName})
	if err != nil {
		return err
	}

	_, err = u.teamsClient.AddTeamMembers(ctx, &teams.AddTeamMembersReq{
		Id:      adminsTeam.Team.Id,
		UserIds: []string{userID},
	})
	return err
}

func isAlreadyExists(err error) bool {
	st, ok := status.FromError(err)
	return ok && st.Code() == codes.AlreadyExists
}
