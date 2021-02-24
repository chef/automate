package adminmgmt

import (
	"context"

	"google.golang.org/grpc/codes"

	grpc_status "google.golang.org/grpc/status"

	iam_req "github.com/chef/automate/api/external/iam/v2/request"
	authz_constants "github.com/chef/automate/components/authz-service/constants"
	"github.com/chef/automate/components/automate-cli/pkg/client"
	"github.com/chef/automate/components/automate-cli/pkg/status"
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

	var membershipID string
	var found bool

	getUserResp, err := apiClient.UsersClient().GetUser(ctx, &iam_req.GetUserReq{
		Id: username,
	})

	s := grpc_status.Convert(err)
	switch s.Code() {
	case codes.NotFound: // attempt to create with desired password
		found = false

		if !dryRun {
			createUserResp, err := apiClient.UsersClient().CreateUser(ctx, &iam_req.CreateUserReq{
				Name:     displayName,
				Id:       username,
				Password: newPassword,
			})
			if err != nil {
				return "", false, wrapUnexpectedError(err, "Failed to create the user")
			}
			membershipID = createUserResp.User.MembershipId
		}

	case codes.OK: // user found, update password
		membershipID = getUserResp.User.MembershipId
		found = true

		if !dryRun {
			_, err = apiClient.UsersClient().UpdateUser(ctx, &iam_req.UpdateUserReq{
				Id:       username,
				Name:     displayName,
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

	return membershipID, found, nil
}

// CreateAdminTeamIfMissing creates the admins team if it is missing.
// It returns a boolean for if it was found or created.
func CreateAdminTeamIfMissing(ctx context.Context,
	apiClient client.APIClient, dryRun bool) (bool, error) {
	return EnsureTeam(ctx, "admins", "admins", apiClient, dryRun)
}

func AddAdminUserToTeam(ctx context.Context,
	apiClient client.APIClient, adminTeamID, adminMembershipID string, dryRun bool) (bool, error) {
	return AddUserToTeam(ctx, apiClient, adminTeamID, adminMembershipID, dryRun)
}

// AddUserToTeam adds the user to a team by its ID, unless they are already in
// the team. It returns a boolean representing whether or not the user needed to
// be added.
func AddUserToTeam(ctx context.Context,
	apiClient client.APIClient, teamID, membershipID string, dryRun bool) (bool, error) {

	getUsersResp, err := apiClient.TeamsClient().GetTeamMembership(ctx, &iam_req.GetTeamMembershipReq{
		Id: teamID,
	})
	if err != nil {
		return false, wrapUnexpectedError(err, "Failed to check team membership")
	}

	addUser := !stringutils.SliceContains(getUsersResp.MembershipIds, membershipID)
	if addUser && !dryRun {
		_, err := apiClient.TeamsClient().AddTeamMembers(ctx, &iam_req.AddTeamMembersReq{
			Id:            teamID,
			MembershipIds: []string{membershipID},
		})
		if err != nil {
			return false, wrapUnexpectedError(err, "Failed to add user to team")
		}
	}

	return addUser, nil
}

// UpdateAdminsPolicyIfNeeded fetches the chef-managed Admin policy's members
// and adds the admins team if it's missing from that list
func UpdateAdminsPolicyIfNeeded(ctx context.Context,
	apiClient client.APIClient, dryRun bool) (bool, error) {
	resp, err := apiClient.PoliciesClient().ListPolicyMembers(ctx, &iam_req.ListPolicyMembersReq{
		Id: authz_constants.AdminPolicyID,
	})
	if err != nil {
		return false, wrapUnexpectedError(err, "Failed to verify members of chef-managed Admin policy")
	}
	found := stringutils.SliceContains(resp.Members, authz_constants.LocalAdminsTeamSubject)

	if !dryRun && !found {
		_, err = apiClient.PoliciesClient().AddPolicyMembers(ctx, &iam_req.AddPolicyMembersReq{
			Id:      authz_constants.AdminPolicyID,
			Members: []string{authz_constants.LocalAdminsTeamSubject},
		})
		if err != nil {
			return false, wrapUnexpectedError(err, "Failed to add local team: admins to chef-managed Admin policy")
		}
		return false, nil
	}

	return found, nil
}

// EnsureTeam creates the desired team if it is missing,
// together with a boolean indicating if the team was created by this call
func EnsureTeam(ctx context.Context,
	id, name string,
	apiClient client.APIClient,
	dryRun bool) (bool, error) {

	found := true
	_, err := apiClient.TeamsClient().GetTeam(ctx, &iam_req.GetTeamReq{
		Id: id,
	})
	if err != nil {
		if grpc_status.Convert(err).Code() == codes.NotFound {
			found = false
		} else {
			return false, wrapUnexpectedError(err, "Failed to retrieve team %q", id)
		}
	}

	if !found && !dryRun {
		_, err := apiClient.TeamsClient().CreateTeam(ctx, &iam_req.CreateTeamReq{
			Id:       id,
			Name:     name,
			Projects: []string{},
		})
		if err != nil {
			return false, wrapUnexpectedError(err, "Failed to create team %q", id)
		}
	}

	return found, nil
}

func wrapUnexpectedError(err error, wrap string, args ...interface{}) error {
	return status.Wrapf(err, status.APIError, wrap, args...)
}
