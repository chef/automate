package adminmgmt

import (
	"context"

	"google.golang.org/grpc/codes"

	grpc_status "google.golang.org/grpc/status"

	authz_constants "github.com/chef/automate/components/authz-service/constants"
	authz_constants_v1 "github.com/chef/automate/components/authz-service/constants/v1"
	authz_constants_v2 "github.com/chef/automate/components/authz-service/constants/v2"
	"github.com/chef/automate/components/automate-cli/pkg/client"
	"github.com/chef/automate/components/automate-cli/pkg/status"
	users_req "github.com/chef/automate/components/automate-gateway/api/auth/users/request"
	"github.com/chef/automate/components/automate-gateway/api/authz"
	authz_req "github.com/chef/automate/components/automate-gateway/api/authz/request"
	v2 "github.com/chef/automate/components/automate-gateway/api/iam/v2"
	authz_v2_req "github.com/chef/automate/components/automate-gateway/api/iam/v2/request"
	"github.com/chef/automate/lib/stringutils"
)

// CreateAdminUserOrUpdatePassword either creates a new admin user or updates
// the existing users password. In either case, it should return the ID and a
// boolean representing whether the user was found or created.
func CreateAdminUserOrUpdatePassword(ctx context.Context,
	apiClient client.APIClient, newAdminPassword string, dryRun bool) (string, bool, error) {
	return CreateUserOrUpdatePassword(ctx, apiClient, "admin", "Local Administrator", newAdminPassword, dryRun)
}

// CreateUserOrUpdatePassword either creates a new user with the supplied
// username or updates the existing user's password. In either case, it returns
// the ID and a boolean representing whether the user was found or created.
func CreateUserOrUpdatePassword(ctx context.Context,
	apiClient client.APIClient, username, displayName, newPassword string, dryRun bool) (string, bool, error) {

	var userID string
	var found bool

	getUserResp, err := apiClient.UsersClient().GetUserByUsername(ctx, &users_req.Username{
		Username: username,
	})

	s := grpc_status.Convert(err)
	switch s.Code() {
	case codes.NotFound: // attempt to create with desired password
		found = false

		if !dryRun {
			createUserResp, err := apiClient.UsersClient().CreateUser(ctx, &users_req.CreateUser{
				Name:     displayName,
				Username: username,
				Password: newPassword,
			})
			if err != nil {
				return "", false, wrapUnexpectedError(err, "Failed to create the user")
			}
			userID = createUserResp.Id
		}

	case codes.OK: // user found, update password
		userID = getUserResp.Id
		found = true

		if !dryRun {
			_, err = apiClient.UsersClient().UpdateUser(ctx, &users_req.UpdateUser{
				Id:       userID,
				Name:     displayName,
				Username: username,
				Password: newPassword,
			})
			if err != nil {
				// The first two args are not-to-be-looked-at by convention, as the err is
				// NOT nil; so, we don't bother returning the userID and true.
				return "", false, wrapUnexpectedError(err, "Failed to update user's password")
			}
		}
	default: // some error occurred querying the user
		return "", false, wrapUnexpectedError(err, "Failed to check if user exists")
	}

	return userID, found, nil
}

// CreateAdminTeamIfMissing creates the admins team if it is missing.
// It returns the team ID in either case and a boolean for if it was
// found or created.
func CreateAdminTeamIfMissing(ctx context.Context,
	apiClient client.APIClient, dryRun bool) (string, bool, error) {
	return EnsureTeam(ctx, "admins", "admins", apiClient, dryRun)
}

func AddAdminUserToTeam(ctx context.Context,
	apiClient client.APIClient, adminTeamID, adminUserID string, dryRun bool) (bool, error) {
	return AddUserToTeam(ctx, apiClient, adminTeamID, adminUserID, dryRun)
}

// AddUserToTeam adds the user to a team by its ID, unless they are already in
// the team. It returns a boolean representing whether or not the user needed to
// be added.
func AddUserToTeam(ctx context.Context,
	apiClient client.APIClient, teamID, userID string, dryRun bool) (bool, error) {

	getUsersResp, err := apiClient.TeamsV2Client().GetTeamMembership(ctx, &authz_v2_req.GetTeamMembershipReq{
		Id: teamID,
	})
	if err != nil {
		return false, wrapUnexpectedError(err, "Failed to check team membership")
	}

	addUser := !stringutils.SliceContains(getUsersResp.UserIds, userID)
	if addUser && !dryRun {
		_, err := apiClient.TeamsV2Client().AddTeamMembers(ctx, &authz_v2_req.AddTeamMembersReq{
			Id:      teamID,
			UserIds: []string{userID},
		})
		if err != nil {
			return false, wrapUnexpectedError(err, "Failed to add user to team")
		}
	}

	return addUser, nil
}

// UpdateV1AdminsPolicyIfNeeded either creates a new admins policy if one doesn't
// exist anymore or creates a new admin policy with the admins team as a subject
//  if the admins team is not already a subject.
// It returns two booleans: one representing if the original policy was found
// and the other representing if a new policy was created.
func UpdateV1AdminsPolicyIfNeeded(ctx context.Context,
	apiClient client.APIClient, dryRun bool) (bool, bool, error) {

	var foundOriginalTeamsPolicy,
		foundTeamInOriginalPolicy,
		createdNewPolicy bool

	policySubjects, foundOriginalTeamsPolicy, err := getOriginalTeamsPolicySubjects(ctx, apiClient.AuthzClient())
	if err != nil {
		return false, false, err
	}

	if foundOriginalTeamsPolicy {
		for _, subject := range policySubjects {
			if subject == authz_constants.LocalAdminsTeamSubject {
				foundTeamInOriginalPolicy = true
				break
			}
		}
	}

	if !foundOriginalTeamsPolicy || !foundTeamInOriginalPolicy {
		createdNewPolicy = true
		if !dryRun {
			_, err := apiClient.AuthzClient().CreatePolicy(ctx, &authz_req.CreatePolicyReq{
				Action:   "*",
				Resource: "*",
				Subjects: []string{authz_constants.LocalAdminsTeamSubject},
			})
			if err != nil {
				return false, false, wrapUnexpectedError(err, "Failed to create new admins policy")
			}
		}
	}

	return foundOriginalTeamsPolicy, createdNewPolicy, nil
}

// UpdateV2AdminsPolicyIfNeeded fetches the chef-managed Admin policy's members
// and adds the admins team if it's missing from that list
func UpdateV2AdminsPolicyIfNeeded(ctx context.Context,
	apiClient client.APIClient, dryRun bool) (bool, error) {
	resp, err := apiClient.PoliciesClient().ListPolicyMembers(ctx, &authz_v2_req.ListPolicyMembersReq{
		Id: authz_constants_v2.AdminPolicyID,
	})
	if err != nil {
		return false, wrapUnexpectedError(err, "Failed to verify members of chef-managed Admin policy")
	}
	found := stringutils.SliceContains(resp.Members, authz_constants.LocalAdminsTeamSubject)

	if !dryRun && !found {
		_, err = apiClient.PoliciesClient().AddPolicyMembers(ctx, &authz_v2_req.AddPolicyMembersReq{
			Id:      authz_constants_v2.AdminPolicyID,
			Members: []string{authz_constants.LocalAdminsTeamSubject},
		})
		if err != nil {
			return false, wrapUnexpectedError(err, "Failed to add local team: admins to chef-managed Admin policy")
		}
		return false, nil
	}

	return found, nil
}

// EnsureTeam creates the desired team if it is missing, and returns the team's
// ID, together with a boolean indicating if the team was created by this call
func EnsureTeam(ctx context.Context,
	id, name string,
	apiClient client.APIClient,
	dryRun bool) (string, bool, error) {

	teamID, found, err := getTeamIDByName(ctx, apiClient.TeamsV2Client(), id)
	if err != nil {
		return "", false, wrapUnexpectedError(err, "Failed to retrieve team %q", id)
	}

	if !found && !dryRun {
		createTeamsResp, err := apiClient.TeamsV2Client().CreateTeam(ctx, &authz_v2_req.CreateTeamReq{
			Id:       id,
			Name:     name,
			Projects: []string{},
		})
		if err != nil {
			return "", false, wrapUnexpectedError(err, "Failed to create team %q", id)
		}

		teamID = createTeamsResp.Team.Id
	}

	return teamID, found, nil
}

func wrapUnexpectedError(err error, wrap string, args ...interface{}) error {
	return status.Wrapf(err, status.APIError, wrap, args...)
}

func getTeamIDByName(ctx context.Context, tc v2.TeamsClient, name string) (string, bool, error) {
	var id string
	var found bool

	listTeamsResp, err := tc.ListTeams(ctx, &authz_v2_req.ListTeamsReq{})
	if err != nil {
		return "", false, wrapUnexpectedError(err, "Failed to retrieve admins team")
	}

	for _, team := range listTeamsResp.Teams {
		if team.Name == name {
			id = team.Id
			found = true
			break
		}
	}

	return id, found, nil
}

func getOriginalTeamsPolicySubjects(ctx context.Context,
	ac authz.AuthorizationClient) ([]string, bool, error) {
	authzListResp, err := ac.ListPolicies(ctx, &authz_req.ListPoliciesReq{})
	if err != nil {
		return nil, false, wrapUnexpectedError(err, "Failed to retrieve policies")
	}

	for _, policy := range authzListResp.Policies {
		if policy.Id == authz_constants_v1.AdminPolicyID {
			return policy.Subjects, true, nil
		}
	}
	return nil, false, nil
}
