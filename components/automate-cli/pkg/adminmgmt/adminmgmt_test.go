package adminmgmt_test

import (
	"context"
	"errors"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/status"

	iam_common "github.com/chef/automate/api/external/iam/v2/common"
	iam_req "github.com/chef/automate/api/external/iam/v2/request"
	iam_resp "github.com/chef/automate/api/external/iam/v2/response"
	authz_constants "github.com/chef/automate/components/authz-service/constants"
	"github.com/chef/automate/components/automate-cli/pkg/adminmgmt"
	"github.com/chef/automate/components/automate-cli/pkg/client/mock"
)

func TestCreateAdminUserOrUpdatePassword(t *testing.T) {
	ctx := context.Background()
	apiClient, serverMocks, err := mock.CreateMockConn(t)
	require.NoError(t, err)
	defer apiClient.CloseConnection()

	pw := "newPassword"

	t.Run("when GetUser fails with an unexpected error then raise that error", func(t *testing.T) {
		serverMocks.UsersMock.GetUserFunc = getUserError(codes.Internal)
		_, _, err := adminmgmt.CreateAdminUserOrUpdatePassword(ctx, apiClient, pw, false)
		require.Error(t, err)
	})

	t.Run("when the admin doesn't exist but creating it throws an error then return the error", func(t *testing.T) {
		serverMocks.UsersMock.GetUserFunc = getUserError(codes.NotFound)
		serverMocks.UsersMock.CreateUserFunc = func(
			_ context.Context, req *iam_req.CreateUserReq) (*iam_resp.CreateUserResp, error) {
			if "Local Administrator" != req.Name || "admin" != req.Id || pw != req.Password {
				return &iam_resp.CreateUserResp{}, nil // this would be unexpected, we want this to return an error
			}
			return nil, status.Error(codes.Internal, "unexpected error")
		}

		_, _, err := adminmgmt.CreateAdminUserOrUpdatePassword(ctx, apiClient, pw, false)
		require.Error(t, err)
	})

	t.Run("when the admin doesn't exist", func(t *testing.T) {
		membershipID := "mocked_id"
		serverMocks.UsersMock.GetUserFunc = getUserError(codes.NotFound)
		serverMocks.UsersMock.CreateUserFunc = func(
			_ context.Context, req *iam_req.CreateUserReq) (*iam_resp.CreateUserResp, error) {

			if "Local Administrator" != req.Name || "admin" != req.Id || pw != req.Password {
				return nil, errors.New("unexpected arguments")
			}
			return &iam_resp.CreateUserResp{
				User: &iam_common.User{
					Id:           "wrong_id",
					Name:         req.Name,
					MembershipId: membershipID,
				},
			}, nil
		}

		t.Run("it returns the membership id and found=false", func(t *testing.T) {
			returnID, returnFound, err := adminmgmt.CreateAdminUserOrUpdatePassword(ctx, apiClient, pw, false)
			require.NoError(t, err)
			assert.Equal(t, membershipID, returnID)
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
		membershipID := "mocked_id"
		serverMocks.UsersMock.GetUserFunc = func(
			_ context.Context, req *iam_req.GetUserReq) (*iam_resp.GetUserResp, error) {

			if "admin" != req.Id {
				return nil, errors.New("unexpected arguments")
			}
			return &iam_resp.GetUserResp{
				User: &iam_common.User{
					Id:           "admin",
					Name:         "Some out of date name",
					MembershipId: membershipID,
				},
			}, nil
		}

		serverMocks.UsersMock.UpdateUserFunc = func(
			_ context.Context, req *iam_req.UpdateUserReq) (*iam_resp.UpdateUserResp, error) {

			if "Local Administrator" != req.Name || "admin" != req.Id || pw != req.Password {
				return nil, errors.New("unexpected arguments")
			}

			return &iam_resp.UpdateUserResp{
				User: &iam_common.User{
					Id:           req.Id,
					Name:         req.Name,
					MembershipId: membershipID,
				},
			}, nil
		}

		t.Run("it returns the membership id and found=true", func(t *testing.T) {
			returnID, returnFound, err := adminmgmt.CreateAdminUserOrUpdatePassword(ctx, apiClient, pw, false)
			require.NoError(t, err)
			assert.Equal(t, membershipID, returnID)
			assert.True(t, returnFound)
		})

		t.Run("when in dry run mode it returns the membership id and found=true", func(t *testing.T) {
			returnID, returnFound, err := adminmgmt.CreateAdminUserOrUpdatePassword(ctx, apiClient, pw, true)
			require.NoError(t, err)
			assert.Equal(t, membershipID, returnID)
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

		_, err := adminmgmt.EnsureTeam(ctx, "admins", "the admin team", apiClient, false)
		require.Error(t, err)
	})

	t.Run("when the admins team does not exist", func(t *testing.T) {
		serverMocks.TeamsMock.ListTeamsFunc = func(
			context.Context, *iam_req.ListTeamsReq) (*iam_resp.ListTeamsResp, error) {

			return &iam_resp.ListTeamsResp{
				Teams: []*iam_common.Team{
					{
						Id:       "mocked-not-admin-id",
						Name:     "not-admin",
						Projects: []string{},
					},
				},
			}, nil
		}

		t.Run("it is created and found=false is returned", func(t *testing.T) {
			createAdminID := "mocked-admin-id"
			serverMocks.TeamsMock.CreateTeamFunc = func(
				_ context.Context, req *iam_req.CreateTeamReq) (*iam_resp.CreateTeamResp, error) {

				if "admins" != req.Name ||
					"admins" != req.Id {
					return nil, errors.New("unexpected arguments")
				}

				return &iam_resp.CreateTeamResp{
					Team: &iam_common.Team{
						Id:       createAdminID,
						Name:     req.Name,
						Projects: []string{},
					},
				}, nil
			}

			serverMocks.TeamsMock.GetTeamFunc = func(
				_ context.Context, req *iam_req.GetTeamReq) (*iam_resp.GetTeamResp, error) {
				return nil, status.Error(codes.NotFound, "Not Found")
			}

			found, err := adminmgmt.CreateAdminTeamIfMissing(ctx, apiClient, false)
			require.NoError(t, err)
			assert.False(t, found)
		})

		t.Run("and dry run mode is on it is not created, id is empty, and found=false are returned", func(t *testing.T) {
			serverMocks.TeamsMock.CreateTeamFunc = createTeamCallUnexpected

			found, err := adminmgmt.EnsureTeam(ctx, "admins", "the admin team", apiClient, true)
			require.NoError(t, err)
			assert.False(t, found)
		})
	})

	t.Run("when the admins team exists already", func(t *testing.T) {
		serverMocks.TeamsMock.GetTeamFunc = func(
			_ context.Context, req *iam_req.GetTeamReq) (*iam_resp.GetTeamResp, error) {
			return &iam_resp.GetTeamResp{
				Team: &iam_common.Team{
					Id:       "admins",
					Name:     "admins",
					Projects: []string{},
				},
			}, nil
		}

		serverMocks.TeamsMock.CreateTeamFunc = createTeamCallUnexpected

		t.Run("it returns found=true", func(t *testing.T) {
			found, err := adminmgmt.EnsureTeam(ctx, "admins", "the admin team", apiClient, false)
			require.NoError(t, err)
			assert.True(t, found)
		})

		t.Run("when in dry run mode it returns found=true", func(t *testing.T) {
			found, err := adminmgmt.EnsureTeam(ctx, "admins", "the admin team", apiClient, false)
			require.NoError(t, err)
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
			_ context.Context, req *iam_req.GetTeamMembershipReq) (*iam_resp.GetTeamMembershipResp, error) {

			if teamsID != req.Id {
				return &iam_resp.GetTeamMembershipResp{}, nil // unexpected arguments
			}
			return nil, status.Error(codes.Internal, "unexpected error")
		}

		_, err := adminmgmt.AddAdminUserToTeam(ctx, apiClient, teamsID, userID, false)
		require.Error(t, err)
	})

	t.Run("when the admin is on the team", func(t *testing.T) {
		serverMocks.TeamsMock.GetTeamMembershipFunc = func(
			_ context.Context, req *iam_req.GetTeamMembershipReq) (*iam_resp.GetTeamMembershipResp, error) {

			if teamsID != req.Id {
				return nil, errors.New("unexpected arguments")
			}
			return &iam_resp.GetTeamMembershipResp{
				MembershipIds: []string{"1", userID, "3"},
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
			_ context.Context, req *iam_req.GetTeamMembershipReq) (*iam_resp.GetTeamMembershipResp, error) {

			if teamsID != req.Id {
				return nil, errors.New("unexpected arguments")
			}
			return &iam_resp.GetTeamMembershipResp{
				MembershipIds: []string{"1", "3"},
			}, nil
		}

		t.Run("it calls AddTeamMembers returns addUser=true", func(t *testing.T) {
			serverMocks.TeamsMock.AddTeamMembersFunc = func(
				_ context.Context, req *iam_req.AddTeamMembersReq) (*iam_resp.AddTeamMembersResp, error) {

				if teamsID != req.Id || len(req.MembershipIds) != 1 || userID != req.MembershipIds[0] {
					return nil, errors.New("unexpected arguments")
				}
				return &iam_resp.AddTeamMembersResp{
					MembershipIds: req.MembershipIds,
				}, nil
			}

			addUser, err := adminmgmt.AddAdminUserToTeam(ctx, apiClient, teamsID, userID, false)
			require.NoError(t, err)
			assert.True(t, addUser)
		})

		t.Run("when AddTeamMembers returns an unexpected error it raises the error", func(t *testing.T) {
			serverMocks.TeamsMock.AddTeamMembersFunc = func(
				_ context.Context, req *iam_req.AddTeamMembersReq) (*iam_resp.AddTeamMembersResp, error) {

				if teamsID != req.Id || len(req.MembershipIds) != 1 || userID != req.MembershipIds[0] {
					return &iam_resp.AddTeamMembersResp{}, nil // unexpected arguments
				}
				return nil, status.Error(codes.Internal, "unexpected error")
			}

			_, err := adminmgmt.AddAdminUserToTeam(ctx, apiClient, teamsID, userID, false)
			require.Error(t, err)
		})

		t.Run("and dry run is enabled it returns addUser=false", func(t *testing.T) {
			serverMocks.TeamsMock.AddTeamMembersFunc = func(
				_ context.Context, req *iam_req.AddTeamMembersReq) (*iam_resp.AddTeamMembersResp, error) {

				return nil, errors.New("unexpected call")
			}

			addUser, err := adminmgmt.AddAdminUserToTeam(ctx, apiClient, teamsID, userID, true)
			require.NoError(t, err)
			assert.True(t, addUser)
		})
	})
}
func TestUpdateAdminsPolicyIfNeeded(t *testing.T) {
	ctx := context.Background()
	apiClient, serverMocks, err := mock.CreateMockConn(t)
	require.NoError(t, err)
	defer apiClient.CloseConnection()

	t.Run("when ListPolicyMembers fails with an unexpected error then raise that error", func(t *testing.T) {
		serverMocks.PoliciesMock.ListPolicyMembersFunc = func(
			context.Context, *iam_req.ListPolicyMembersReq) (*iam_resp.ListPolicyMembersResp, error) {
			return nil, status.Error(codes.Internal, "unexpected error")
		}

		_, err := adminmgmt.UpdateAdminsPolicyIfNeeded(ctx, apiClient, false)
		require.Error(t, err)
	})

	t.Run("when AddPolicyMembers fails with an unexpected error then raise that error", func(t *testing.T) {
		serverMocks.PoliciesMock.AddPolicyMembersFunc = func(
			context.Context, *iam_req.AddPolicyMembersReq) (*iam_resp.AddPolicyMembersResp, error) {
			return nil, status.Error(codes.Internal, "unexpected error")
		}

		_, err := adminmgmt.UpdateAdminsPolicyIfNeeded(ctx, apiClient, false)
		require.Error(t, err)
	})

	t.Run("returns true (found) when ListPolicyMembers returns with admins team in Admin policy", func(t *testing.T) {
		serverMocks.PoliciesMock.ListPolicyMembersFunc = func(
			context.Context, *iam_req.ListPolicyMembersReq) (*iam_resp.ListPolicyMembersResp, error) {
			return &iam_resp.ListPolicyMembersResp{
				Members: []string{authz_constants.LocalAdminsTeamSubject},
			}, nil
		}

		foundInPolicy, err := adminmgmt.UpdateAdminsPolicyIfNeeded(ctx, apiClient, false)
		require.NoError(t, err)
		assert.True(t, foundInPolicy)
	})

	t.Run("returns false (not found) when admins team must be added to Admin policy", func(t *testing.T) {
		serverMocks.PoliciesMock.ListPolicyMembersFunc = func(
			context.Context, *iam_req.ListPolicyMembersReq) (*iam_resp.ListPolicyMembersResp, error) {
			return &iam_resp.ListPolicyMembersResp{
				Members: []string{},
			}, nil
		}

		serverMocks.PoliciesMock.AddPolicyMembersFunc = func(
			context.Context, *iam_req.AddPolicyMembersReq) (*iam_resp.AddPolicyMembersResp, error) {
			return &iam_resp.AddPolicyMembersResp{
				Members: []string{authz_constants.LocalAdminsTeamSubject},
			}, nil
		}

		foundInPolicy, err := adminmgmt.UpdateAdminsPolicyIfNeeded(ctx, apiClient, false)
		require.NoError(t, err)
		assert.False(t, foundInPolicy)
	})
}

func getUserError(c codes.Code) func(context.Context, *iam_req.GetUserReq) (*iam_resp.GetUserResp, error) {
	return func(context.Context, *iam_req.GetUserReq) (*iam_resp.GetUserResp, error) {
		return nil, status.Error(c, "unexpected error")
	}
}

func listTeamsError(c codes.Code) func(context.Context, *iam_req.ListTeamsReq) (*iam_resp.ListTeamsResp, error) {
	return func(context.Context, *iam_req.ListTeamsReq) (*iam_resp.ListTeamsResp, error) {
		return nil, status.Error(c, "unexpected error")
	}
}

func createTeamCallUnexpected(
	context.Context, *iam_req.CreateTeamReq) (*iam_resp.CreateTeamResp, error) {
	return nil, errors.New("unexpected call")
}
