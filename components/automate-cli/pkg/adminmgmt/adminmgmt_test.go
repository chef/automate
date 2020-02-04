package adminmgmt_test

import (
	"context"
	"errors"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/status"

	authz_constants "github.com/chef/automate/components/authz-service/constants"
	authz_constants_v1 "github.com/chef/automate/components/authz-service/constants/v1"
	"github.com/chef/automate/components/automate-cli/pkg/adminmgmt"
	"github.com/chef/automate/components/automate-cli/pkg/client/mock"
	users_req "github.com/chef/automate/components/automate-gateway/api/auth/users/request"
	users_resp "github.com/chef/automate/components/automate-gateway/api/auth/users/response"
	authz_req "github.com/chef/automate/components/automate-gateway/api/authz/request"
	authz_resp "github.com/chef/automate/components/automate-gateway/api/authz/response"
	teams_common "github.com/chef/automate/components/automate-gateway/api/iam/v2/common"
	policies_req "github.com/chef/automate/components/automate-gateway/api/iam/v2/request"
	teams_req "github.com/chef/automate/components/automate-gateway/api/iam/v2/request"
	policies_resp "github.com/chef/automate/components/automate-gateway/api/iam/v2/response"
	teams_resp "github.com/chef/automate/components/automate-gateway/api/iam/v2/response"
)

func TestCreateAdminUserOrUpdatePassword(t *testing.T) {
	ctx := context.Background()
	apiClient, serverMocks, err := mock.CreateMockConn(t)
	require.NoError(t, err)
	defer apiClient.CloseConnection()

	pw := "newPassword"

	t.Run("when GetUserByUsername fails with an unexpected error then raise that error", func(t *testing.T) {
		serverMocks.UsersMock.GetUserByUsernameFunc = getUserByUsernameError(codes.Internal)
		_, _, err := adminmgmt.CreateAdminUserOrUpdatePassword(ctx, apiClient, pw, false)
		require.Error(t, err)
	})

	t.Run("when the admin doesn't exist but creating it throws an error then return the error", func(t *testing.T) {
		serverMocks.UsersMock.GetUserByUsernameFunc = getUserByUsernameError(codes.NotFound)
		serverMocks.UsersMock.CreateUserFunc = func(
			_ context.Context, req *users_req.CreateUser) (*users_resp.User, error) {
			if "Local Administrator" != req.Name || "admin" != req.Username || pw != req.Password {
				return &users_resp.User{}, nil // this would be unexpected, we want this to return an error
			}
			return nil, status.Error(codes.Internal, "unexpected error")
		}

		_, _, err := adminmgmt.CreateAdminUserOrUpdatePassword(ctx, apiClient, pw, false)
		require.Error(t, err)
	})

	t.Run("when the admin doesn't exist", func(t *testing.T) {
		id := "mocked_id"
		serverMocks.UsersMock.GetUserByUsernameFunc = getUserByUsernameError(codes.NotFound)
		serverMocks.UsersMock.CreateUserFunc = func(
			_ context.Context, req *users_req.CreateUser) (*users_resp.User, error) {

			if "Local Administrator" != req.Name || "admin" != req.Username || pw != req.Password {
				return nil, errors.New("unexpected arguments")
			}
			return &users_resp.User{
				Id:       id,
				Name:     req.Name,
				Username: req.Username,
			}, nil
		}

		t.Run("it returns the id and found=false", func(t *testing.T) {
			returnID, returnFound, err := adminmgmt.CreateAdminUserOrUpdatePassword(ctx, apiClient, pw, false)
			require.NoError(t, err)
			assert.Equal(t, id, returnID)
			assert.False(t, returnFound)
		})

		t.Run("when in dry run mode it returns an empty ID and found=false", func(t *testing.T) {
			returnID, returnFound, err := adminmgmt.CreateAdminUserOrUpdatePassword(ctx, apiClient, pw, true)
			require.NoError(t, err)
			assert.Equal(t, "", returnID)
			assert.False(t, returnFound)
		})

	})

	t.Run("when the admin is found", func(t *testing.T) {
		id := "mocked_id"
		serverMocks.UsersMock.GetUserByUsernameFunc = func(
			_ context.Context, req *users_req.Username) (*users_resp.User, error) {

			if "admin" != req.Username {
				return nil, errors.New("unexpected arguments")
			}
			return &users_resp.User{
				Id:       id,
				Name:     "Some out of date name",
				Username: "admin",
			}, nil
		}

		serverMocks.UsersMock.UpdateUserFunc = func(
			_ context.Context, req *users_req.UpdateUser) (*users_resp.User, error) {

			if "Local Administrator" != req.Name || "admin" != req.Username || pw != req.Password {
				return nil, errors.New("unexpected arguments")
			}

			return &users_resp.User{
				Id:       id,
				Name:     req.Name,
				Username: req.Username,
			}, nil
		}

		t.Run("it returns the id and found=true", func(t *testing.T) {
			returnID, returnFound, err := adminmgmt.CreateAdminUserOrUpdatePassword(ctx, apiClient, pw, false)
			require.NoError(t, err)
			assert.Equal(t, id, returnID)
			assert.True(t, returnFound)
		})

		t.Run("when in dry run mode it returns the id and found=true", func(t *testing.T) {
			returnID, returnFound, err := adminmgmt.CreateAdminUserOrUpdatePassword(ctx, apiClient, pw, true)
			require.NoError(t, err)
			assert.Equal(t, id, returnID)
			assert.True(t, returnFound)
		})
	})
}

func TestEnsureTeam(t *testing.T) {
	ctx := context.Background()
	apiClient, serverMocks, err := mock.CreateMockConn(t)
	require.NoError(t, err)
	defer apiClient.CloseConnection()

	t.Run("when ListTeams fails with an unexpected error then raise that error", func(t *testing.T) {
		serverMocks.TeamsMock.ListTeamsFunc = listTeamsError(codes.Internal)

		_, _, err := adminmgmt.EnsureTeam(ctx, "admins", "the admin team", apiClient, false)
		require.Error(t, err)
	})

	t.Run("when the admins team does not exist", func(t *testing.T) {
		serverMocks.TeamsMock.ListTeamsFunc = func(
			context.Context, *teams_req.ListTeamsReq) (*teams_resp.ListTeamsResp, error) {

			return &teams_resp.ListTeamsResp{
				Teams: []*teams_common.Team{
					{
						Id:       "mocked-not-admin-id",
						Name:     "not-admin",
						Projects: []string{},
					},
				},
			}, nil
		}

		t.Run("it is created and its id and found=false are returned", func(t *testing.T) {
			createAdminID := "mocked-admin-id"
			serverMocks.TeamsMock.CreateTeamFunc = func(
				_ context.Context, req *teams_req.CreateTeamReq) (*teams_resp.CreateTeamResp, error) {

				if "admins" != req.Name ||
					"admins" != req.Id {
					return nil, errors.New("unexpected arguments")
				}

				return &teams_resp.CreateTeamResp{
					Team: &teams_common.Team{
						Id:       createAdminID,
						Name:     req.Name,
						Projects: []string{},
					},
				}, nil
			}

			id, found, err := adminmgmt.CreateAdminTeamIfMissing(ctx, apiClient, false)
			require.NoError(t, err)
			assert.Equal(t, createAdminID, id)
			assert.False(t, found)
		})

		t.Run("and dry run mode is on it is not created, id is empty, and found=false are returned", func(t *testing.T) {
			serverMocks.TeamsMock.CreateTeamFunc = createTeamCallUnexpected

			id, found, err := adminmgmt.EnsureTeam(ctx, "admins", "the admin team", apiClient, true)
			require.NoError(t, err)
			assert.Equal(t, "", id)
			assert.False(t, found)
		})
	})

	t.Run("when the admins team exists already", func(t *testing.T) {
		mockedAdminsID := "mocked-admin-id"
		serverMocks.TeamsMock.ListTeamsFunc = func(
			context.Context, *teams_req.ListTeamsReq) (*teams_resp.ListTeamsResp, error) {

			return &teams_resp.ListTeamsResp{
				Teams: []*teams_common.Team{
					{
						Id:       "mocked-not-admin-id",
						Name:     "not-admin",
						Projects: []string{},
					},
					{
						Id:       mockedAdminsID,
						Name:     "admins",
						Projects: []string{},
					},
				},
			}, nil
		}

		serverMocks.TeamsMock.CreateTeamFunc = createTeamCallUnexpected

		t.Run("it returns the team id and found=true", func(t *testing.T) {
			id, found, err := adminmgmt.EnsureTeam(ctx, "admins", "the admin team", apiClient, false)
			require.NoError(t, err)
			assert.Equal(t, mockedAdminsID, id)
			assert.True(t, found)
		})

		t.Run("when in dry run mode it returns and empty id and found=true", func(t *testing.T) {
			id, found, err := adminmgmt.EnsureTeam(ctx, "admins", "the admin team", apiClient, false)
			require.NoError(t, err)
			assert.Equal(t, mockedAdminsID, id)
			assert.True(t, found)
		})
	})
}
func TestAddAdminUserToTeam(t *testing.T) {
	ctx := context.Background()
	apiClient, serverMocks, err := mock.CreateMockConn(t)
	require.NoError(t, err)
	defer apiClient.CloseConnection()

	teamsID := "teams-id"
	userID := "user-id"
	t.Run("when GetTeamMembership fails with an unexpected error then raise that error", func(t *testing.T) {
		serverMocks.TeamsMock.GetTeamMembershipFunc = func(
			_ context.Context, req *teams_req.GetTeamMembershipReq) (*teams_resp.GetTeamMembershipResp, error) {

			if teamsID != req.Id {
				return &teams_resp.GetTeamMembershipResp{}, nil // unexpected arguments
			}
			return nil, status.Error(codes.Internal, "unexpected error")
		}

		_, err := adminmgmt.AddAdminUserToTeam(ctx, apiClient, teamsID, userID, false)
		require.Error(t, err)
	})

	t.Run("when the admin is on the team", func(t *testing.T) {
		serverMocks.TeamsMock.GetTeamMembershipFunc = func(
			_ context.Context, req *teams_req.GetTeamMembershipReq) (*teams_resp.GetTeamMembershipResp, error) {

			if teamsID != req.Id {
				return nil, errors.New("unexpected arguments")
			}
			return &teams_resp.GetTeamMembershipResp{
				UserIds: []string{"1", userID, "3"},
			}, nil
		}

		t.Run("it returns addUser=false", func(t *testing.T) {
			addUser, err := adminmgmt.AddAdminUserToTeam(ctx, apiClient, teamsID, userID, false)
			require.NoError(t, err)
			assert.False(t, addUser)
		})

		t.Run("and dry run is enabled it returns addUser=false", func(t *testing.T) {
			addUser, err := adminmgmt.AddAdminUserToTeam(ctx, apiClient, teamsID, userID, true)
			require.NoError(t, err)
			assert.False(t, addUser)
		})
	})

	t.Run("when the admin is not on the team", func(t *testing.T) {
		serverMocks.TeamsMock.GetTeamMembershipFunc = func(
			_ context.Context, req *teams_req.GetTeamMembershipReq) (*teams_resp.GetTeamMembershipResp, error) {

			if teamsID != req.Id {
				return nil, errors.New("unexpected arguments")
			}
			return &teams_resp.GetTeamMembershipResp{
				UserIds: []string{"1", "3"},
			}, nil
		}

		t.Run("it calls AddTeamMembers returns addUser=true", func(t *testing.T) {
			serverMocks.TeamsMock.AddTeamMembersFunc = func(
				_ context.Context, req *teams_req.AddTeamMembersReq) (*teams_resp.AddTeamMembersResp, error) {

				if teamsID != req.Id || len(req.UserIds) != 1 || userID != req.UserIds[0] {
					return nil, errors.New("unexpected arguments")
				}
				return &teams_resp.AddTeamMembersResp{
					// TODO
					UserIds: []string{},
				}, nil
			}

			addUser, err := adminmgmt.AddAdminUserToTeam(ctx, apiClient, teamsID, userID, false)
			require.NoError(t, err)
			assert.True(t, addUser)
		})

		t.Run("when AddTeamMembers returns an unexpected error it raises the error", func(t *testing.T) {
			serverMocks.TeamsMock.AddTeamMembersFunc = func(
				_ context.Context, req *teams_req.AddTeamMembersReq) (*teams_resp.AddTeamMembersResp, error) {

				if teamsID != req.Id || len(req.UserIds) != 1 || userID != req.UserIds[0] {
					return &teams_resp.AddTeamMembersResp{}, nil // unexpected arguments
				}
				return nil, status.Error(codes.Internal, "unexpected error")
			}

			_, err := adminmgmt.AddAdminUserToTeam(ctx, apiClient, teamsID, userID, false)
			require.Error(t, err)
		})

		t.Run("and dry run is enabled it returns addUser=false", func(t *testing.T) {
			serverMocks.TeamsMock.AddTeamMembersFunc = func(
				context.Context, *teams_req.AddTeamMembersReq) (*teams_resp.AddTeamMembersResp, error) {

				return nil, errors.New("unexpected call")
			}

			addUser, err := adminmgmt.AddAdminUserToTeam(ctx, apiClient, teamsID, userID, true)
			require.NoError(t, err)
			assert.True(t, addUser)
		})
	})
}

func TestUpdateV1AdminsPolicyIfNeeded(t *testing.T) {
	ctx := context.Background()
	apiClient, serverMocks, err := mock.CreateMockConn(t)
	require.NoError(t, err)
	defer apiClient.CloseConnection()

	t.Run("when ListPolicies fails with an unexpected error then raise that error", func(t *testing.T) {
		serverMocks.AuthzMock.ListPoliciesFunc = func(
			context.Context, *authz_req.ListPoliciesReq) (*authz_resp.ListPoliciesResp, error) {
			return nil, status.Error(codes.Internal, "unexpected error")
		}

		_, _, err := adminmgmt.UpdateV1AdminsPolicyIfNeeded(ctx, apiClient, false)
		require.Error(t, err)
	})

	t.Run("when the original admins policy does not exist it returns false, true, nil", func(t *testing.T) {
		serverMocks.AuthzMock.ListPoliciesFunc = func(
			context.Context, *authz_req.ListPoliciesReq) (*authz_resp.ListPoliciesResp, error) {

			return &authz_resp.ListPoliciesResp{
				Policies: []*authz_resp.Policy{
					{
						Id:       "wrong-id",
						Subjects: []string{authz_constants.LocalAdminsTeamSubject, "some:other:subject"},
					},
				},
			}, nil
		}

		serverMocks.AuthzMock.CreatePolicyFunc = createDefaultAdminPolicy

		foundPolicy, createdNewPolicy, err := adminmgmt.UpdateV1AdminsPolicyIfNeeded(ctx, apiClient, false)
		require.NoError(t, err)
		assert.False(t, foundPolicy)
		assert.True(t, createdNewPolicy)
	})

	t.Run("when the original admins policy exists and contains the admins team it returns true, false, nil", func(t *testing.T) {
		serverMocks.AuthzMock.ListPoliciesFunc = func(
			context.Context, *authz_req.ListPoliciesReq) (*authz_resp.ListPoliciesResp, error) {

			return &authz_resp.ListPoliciesResp{
				Policies: []*authz_resp.Policy{
					{
						Id:       authz_constants_v1.AdminPolicyID,
						Subjects: []string{authz_constants.LocalAdminsTeamSubject, "some:other:subject"},
					},
				},
			}, nil
		}

		serverMocks.AuthzMock.CreatePolicyFunc = func(
			context.Context, *authz_req.CreatePolicyReq) (*authz_resp.CreatePolicyResp, error) {
			return nil, errors.New("unexpected call")
		}

		foundPolicy, createdNewPolicy, err := adminmgmt.UpdateV1AdminsPolicyIfNeeded(ctx, apiClient, false)
		require.NoError(t, err)
		assert.True(t, foundPolicy)
		assert.False(t, createdNewPolicy)
	})

	t.Run("when the original admins policy exists and does not contain the admins team it returns true, true, nil", func(t *testing.T) {
		serverMocks.AuthzMock.ListPoliciesFunc = func(
			context.Context, *authz_req.ListPoliciesReq) (*authz_resp.ListPoliciesResp, error) {

			return &authz_resp.ListPoliciesResp{
				Policies: []*authz_resp.Policy{
					{
						Id:       authz_constants_v1.AdminPolicyID,
						Subjects: []string{"some:other:subject"},
					},
				},
			}, nil
		}
		serverMocks.AuthzMock.CreatePolicyFunc = createDefaultAdminPolicy

		foundPolicy, createdNewPolicy, err := adminmgmt.UpdateV1AdminsPolicyIfNeeded(ctx, apiClient, false)
		require.NoError(t, err)
		assert.True(t, foundPolicy)
		assert.True(t, createdNewPolicy)
	})

	t.Run("when the original admins policy exists and does not contain the admins team it returns true, true, nil", func(t *testing.T) {
		serverMocks.AuthzMock.ListPoliciesFunc = func(
			context.Context, *authz_req.ListPoliciesReq) (*authz_resp.ListPoliciesResp, error) {

			return &authz_resp.ListPoliciesResp{
				Policies: []*authz_resp.Policy{
					{
						Id:       authz_constants_v1.AdminPolicyID,
						Subjects: []string{"some:other:subject"},
					},
				},
			}, nil
		}
		serverMocks.AuthzMock.CreatePolicyFunc = createDefaultAdminPolicy

		foundPolicy, createdNewPolicy, err := adminmgmt.UpdateV1AdminsPolicyIfNeeded(ctx, apiClient, false)
		require.NoError(t, err)
		assert.True(t, foundPolicy)
		assert.True(t, createdNewPolicy)
	})
}

func TestUpdateV2AdminsPolicyIfNeeded(t *testing.T) {
	ctx := context.Background()
	apiClient, serverMocks, err := mock.CreateMockConn(t)
	require.NoError(t, err)
	defer apiClient.CloseConnection()

	t.Run("when ListPolicyMembers fails with an unexpected error then raise that error", func(t *testing.T) {
		serverMocks.PoliciesMock.ListPolicyMembersFunc = func(
			context.Context, *policies_req.ListPolicyMembersReq) (*policies_resp.ListPolicyMembersResp, error) {
			return nil, status.Error(codes.Internal, "unexpected error")
		}

		_, err := adminmgmt.UpdateV2AdminsPolicyIfNeeded(ctx, apiClient, false)
		require.Error(t, err)
	})

	t.Run("when AddPolicyMembers fails with an unexpected error then raise that error", func(t *testing.T) {
		serverMocks.PoliciesMock.AddPolicyMembersFunc = func(
			context.Context, *policies_req.AddPolicyMembersReq) (*policies_resp.AddPolicyMembersResp, error) {
			return nil, status.Error(codes.Internal, "unexpected error")
		}

		_, err := adminmgmt.UpdateV2AdminsPolicyIfNeeded(ctx, apiClient, false)
		require.Error(t, err)
	})

	t.Run("returns true (found) when ListPolicyMembers returns with admins team in Admin policy", func(t *testing.T) {
		serverMocks.PoliciesMock.ListPolicyMembersFunc = func(
			context.Context, *policies_req.ListPolicyMembersReq) (*policies_resp.ListPolicyMembersResp, error) {
			return &policies_resp.ListPolicyMembersResp{
				Members: []string{authz_constants.LocalAdminsTeamSubject},
			}, nil
		}

		foundInPolicy, err := adminmgmt.UpdateV2AdminsPolicyIfNeeded(ctx, apiClient, false)
		require.NoError(t, err)
		assert.True(t, foundInPolicy)
	})

	t.Run("returns false (not found) when admins team must be added to Admin policy", func(t *testing.T) {
		serverMocks.PoliciesMock.ListPolicyMembersFunc = func(
			context.Context, *policies_req.ListPolicyMembersReq) (*policies_resp.ListPolicyMembersResp, error) {
			return &policies_resp.ListPolicyMembersResp{
				Members: []string{},
			}, nil
		}

		serverMocks.PoliciesMock.AddPolicyMembersFunc = func(
			context.Context, *policies_req.AddPolicyMembersReq) (*policies_resp.AddPolicyMembersResp, error) {
			return &policies_resp.AddPolicyMembersResp{
				Members: []string{authz_constants.LocalAdminsTeamSubject},
			}, nil
		}

		foundInPolicy, err := adminmgmt.UpdateV2AdminsPolicyIfNeeded(ctx, apiClient, false)
		require.NoError(t, err)
		assert.False(t, foundInPolicy)
	})
}

func getUserByUsernameError(c codes.Code) func(context.Context, *users_req.Username) (*users_resp.User, error) {
	return func(context.Context, *users_req.Username) (*users_resp.User, error) {
		return nil, status.Error(c, "unexpected error")
	}
}

func listTeamsError(c codes.Code) func(context.Context, *teams_req.ListTeamsReq) (*teams_resp.ListTeamsResp, error) {
	return func(context.Context, *teams_req.ListTeamsReq) (*teams_resp.ListTeamsResp, error) {
		return nil, status.Error(c, "unexpected error")
	}
}

func createTeamCallUnexpected(
	context.Context, *teams_req.CreateTeamReq) (*teams_resp.CreateTeamResp, error) {
	return nil, errors.New("unexpected call")
}

func createDefaultAdminPolicy(_ context.Context, req *authz_req.CreatePolicyReq) (*authz_resp.CreatePolicyResp, error) {
	if req.Action != "*" || req.Resource != "*" || len(req.Subjects) != 1 ||
		req.Subjects[0] != authz_constants.LocalAdminsTeamSubject {
		return nil, errors.New("unexpected arguments")
	}
	// We don't care about content of response, just that no error was returned.
	return &authz_resp.CreatePolicyResp{}, nil
}
