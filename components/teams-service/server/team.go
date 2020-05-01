package server

import (
	"context"

	"github.com/chef/automate/lib/version"
	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/status"

	ver_api "github.com/chef/automate/api/external/common/version"
	"github.com/chef/automate/api/interservice/authz"
	"github.com/chef/automate/api/interservice/teams"
	"github.com/chef/automate/components/teams-service/service"
	"github.com/chef/automate/components/teams-service/storage"
)

// TeamServer is a teams server
type TeamServer struct {
	service *service.Service
}

// NewTeamServer returns a Teams server
func NewTeamServer(service *service.Service) *TeamServer {
	return &TeamServer{service: service}
}

// GetVersion returns the version of Teams GRPC API
func (t *TeamServer) GetVersion(
	ctx context.Context,
	_ *ver_api.VersionInfoRequest) (*ver_api.VersionInfo, error) {
	return &ver_api.VersionInfo{
		Name:    "teams-service",
		Version: version.Version,
		Sha:     version.GitSHA,
		Built:   version.BuildTime,
	}, nil
}

// GetTeam takes an ID and returns a Team object
func (t *TeamServer) GetTeam(ctx context.Context, req *teams.GetTeamReq) (*teams.GetTeamResp, error) {
	ctx, cancel := context.WithCancel(ctx)
	defer cancel()

	team, err := t.service.Storage.GetTeam(ctx, req.Id)
	if err != nil {
		return nil, service.ParseStorageError(err, req.Id, "team")
	}

	return &teams.GetTeamResp{
		Team: fromStorage(team),
	}, nil
}

// ListTeams returns a list of teams from the db
func (t *TeamServer) ListTeams(ctx context.Context, req *teams.ListTeamsReq) (*teams.ListTeamsResp, error) {
	ctx, cancel := context.WithCancel(ctx)
	defer cancel()

	teamsList, err := t.service.Storage.GetTeams(ctx)
	if err != nil {
		return nil, status.Error(codes.Internal, err.Error())
	}

	return &teams.ListTeamsResp{
		Teams: fromStorageToList(teamsList),
	}, nil
}

// CreateTeam creates a new team
func (t *TeamServer) CreateTeam(ctx context.Context,
	req *teams.CreateTeamReq) (*teams.CreateTeamResp, error) {

	ctx, cancel := context.WithCancel(ctx)
	defer cancel()

	var team storage.Team
	var err error
	if team, err = t.service.Storage.StoreTeam(ctx, req.Id, req.Name, req.Projects); err != nil {
		// if the error is already a GRPC status code, return that directly.
		if _, ok := status.FromError(err); ok {
			return nil, err
		}
		if err == storage.ErrConflict {
			return nil, status.Errorf(codes.AlreadyExists, "team with ID %q already exists", req.Id)
		}
		return nil, status.Error(codes.Internal, err.Error())
	}

	return &teams.CreateTeamResp{
		Team: fromStorage(team),
	}, nil
}

// DeleteTeam deletes a team from the db
func (t *TeamServer) DeleteTeam(ctx context.Context, req *teams.DeleteTeamReq) (*teams.DeleteTeamResp, error) {
	ctx, cancel := context.WithCancel(ctx)
	defer cancel()

	team, err := t.service.Storage.DeleteTeam(ctx, req.Id)
	if err != nil {
		return nil, service.ParseStorageError(err, req.Id, "team")
	}

	teamSubject := "team:local:" + team.ID
	_, err = t.service.AuthzClient.PurgeSubjectFromPolicies(ctx, &authz.PurgeSubjectFromPoliciesReq{
		Subject: teamSubject,
	})
	if err != nil {
		t.service.Logger.Warnf("failed to purge subjects on team delete: %s", err.Error())
		return nil, status.Errorf(codes.Internal, "failed to purge team %q from policies: %s", req.Id, err.Error())
	}

	return &teams.DeleteTeamResp{
		Team: fromStorage(team),
	}, nil
}

// UpdateTeam updates a team in the db via post
func (t *TeamServer) UpdateTeam(ctx context.Context, req *teams.UpdateTeamReq) (*teams.UpdateTeamResp, error) {
	ctx, cancel := context.WithCancel(ctx)
	defer cancel()

	team, err := t.service.Storage.EditTeam(ctx, req.Id, req.Name, req.Projects)
	if err != nil {
		// if the error is already a GRPC status code, return that directly.
		if _, ok := status.FromError(err); ok {
			return nil, err
		}
		return nil, service.ParseStorageError(err, req.Id, "team")
	}

	return &teams.UpdateTeamResp{
		Team: fromStorage(team),
	}, nil
}

// AddTeamMembers associates an array of members with an existing team
func (t *TeamServer) AddTeamMembers(ctx context.Context,
	req *teams.AddTeamMembersReq) (*teams.AddTeamMembersResp, error) {

	if len(req.UserIds) == 0 {
		return nil, status.Error(codes.InvalidArgument, "missing user IDs")
	}

	updatedUserIDs, err := t.service.Storage.AddUsers(ctx, req.Id, req.UserIds)
	if err != nil && err != storage.ErrConflict {
		return nil, service.ParseStorageError(err, req.Id, "user")
	}

	return &teams.AddTeamMembersResp{
		UserIds: updatedUserIDs,
	}, nil
}

// RemoveTeamMembers disassociates an array of members with an existing team.
func (t *TeamServer) RemoveTeamMembers(ctx context.Context,
	req *teams.RemoveTeamMembersReq) (*teams.RemoveTeamMembersResp, error) {

	updatedUserIDs, err := t.service.Storage.RemoveUsers(ctx, req.Id, req.UserIds)
	if err != nil {
		return nil, service.ParseStorageError(err, req.Id, "team")
	}

	return &teams.RemoveTeamMembersResp{
		UserIds: updatedUserIDs,
	}, nil
}

// GetTeamsForMember fetches a list of a members's associated teams
func (t *TeamServer) GetTeamsForMember(
	ctx context.Context,
	req *teams.GetTeamsForMemberReq,
) (*teams.GetTeamsForMemberResp, error) {

	teamList, err := t.service.Storage.GetTeamsForUser(ctx, req.UserId)
	if err != nil {
		return nil, service.ParseStorageError(err, req.UserId, "user")
	}

	return &teams.GetTeamsForMemberResp{
		Teams: fromStorageToList(teamList),
	}, nil
}

// GetTeamMembership fetches a list of member ids associated with a team
func (t *TeamServer) GetTeamMembership(ctx context.Context,
	req *teams.GetTeamMembershipReq) (*teams.GetTeamMembershipResp, error) {

	// verify team exists and isn't filtered out by the project filter
	_, err := t.service.Storage.GetTeam(ctx, req.Id)
	if err != nil {
		return nil, service.ParseStorageError(err, req.Id, "team")
	}

	userIDs, err := t.service.Storage.GetUserIDsForTeam(ctx, req.Id)
	if err != nil {
		return nil, service.ParseStorageError(err, req.Id, "team")
	}

	return &teams.GetTeamMembershipResp{
		UserIds: userIDs,
	}, err
}

// GRPC API functions only used internally (not used in gateway).

// PurgeUserMembership removes the user's membership from all teams to which
// the user belongs and returns that list of teams
func (t *TeamServer) PurgeUserMembership(ctx context.Context,
	req *teams.PurgeUserMembershipReq) (*teams.PurgeUserMembershipResp, error) {

	if req.UserId == "" {
		return nil, status.Error(codes.InvalidArgument, "invalid userId")
	}

	teamIDs, err := t.service.Storage.PurgeUserMembership(ctx, req.UserId)
	if err != nil {
		return nil, service.ParseStorageError(err, req.UserId, "user")
	}

	return &teams.PurgeUserMembershipResp{
		Ids: teamIDs,
	}, nil
}

func fromStorage(s storage.Team) *teams.Team {
	return &teams.Team{
		Id:       s.ID,
		Name:     s.Name,
		Projects: s.Projects,
	}
}

func fromStorageToList(sl []storage.Team) []*teams.Team {
	tl := make([]*teams.Team, len(sl))

	for i, team := range sl {
		tl[i] = fromStorage(team)
	}

	return tl
}
