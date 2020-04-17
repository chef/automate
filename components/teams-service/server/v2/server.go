package v2

import (
	"context"

	"github.com/chef/automate/lib/version"
	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/status"

	ver_api "github.com/chef/automate/api/external/common/version"
	authz "github.com/chef/automate/api/interservice/authz/common"
	teams "github.com/chef/automate/api/interservice/teams/v2"
	"github.com/chef/automate/components/teams-service/service"
	"github.com/chef/automate/components/teams-service/storage"
)

// Server is a V2 teams server
type Server struct {
	service *service.Service
}

// NewServer returns a V2 Teams server
func NewServer(service *service.Service) *Server {
	return &Server{service: service}
}

// GetVersion returns the version of Teams GRPC API
func (s *Server) GetVersion(
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
func (s *Server) GetTeam(ctx context.Context, req *teams.GetTeamReq) (*teams.GetTeamResp, error) {
	ctx, cancel := context.WithCancel(ctx)
	defer cancel()

	team, err := s.service.Storage.GetTeam(ctx, req.Id)
	if err != nil {
		return nil, service.ParseStorageError(err, req.Id, "team")
	}

	return &teams.GetTeamResp{
		Team: fromStorage(team),
	}, nil
}

// ListTeams returns a list of teams from the db
func (s *Server) ListTeams(ctx context.Context, req *teams.ListTeamsReq) (*teams.ListTeamsResp, error) {
	ctx, cancel := context.WithCancel(ctx)
	defer cancel()

	teamsList, err := s.service.Storage.GetTeams(ctx)
	if err != nil {
		return nil, status.Error(codes.Internal, err.Error())
	}

	return &teams.ListTeamsResp{
		Teams: fromStorageToList(teamsList),
	}, nil
}

// CreateTeam creates a new team
func (s *Server) CreateTeam(ctx context.Context,
	req *teams.CreateTeamReq) (*teams.CreateTeamResp, error) {

	ctx, cancel := context.WithCancel(ctx)
	defer cancel()

	var team storage.Team
	var err error
	if team, err = s.service.Storage.StoreTeam(ctx, req.Id, req.Name, req.Projects); err != nil {
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
func (s *Server) DeleteTeam(ctx context.Context, req *teams.DeleteTeamReq) (*teams.DeleteTeamResp, error) {
	ctx, cancel := context.WithCancel(ctx)
	defer cancel()

	team, err := s.service.Storage.DeleteTeam(ctx, req.Id)
	if err != nil {
		return nil, service.ParseStorageError(err, req.Id, "team")
	}

	teamSubject := "team:local:" + team.ID
	_, err = s.service.AuthzClient.PurgeSubjectFromPolicies(ctx, &authz.PurgeSubjectFromPoliciesReq{
		Subject: teamSubject,
	})
	if err != nil {
		s.service.Logger.Warnf("failed to purge subjects on team delete: %s", err.Error())
		return nil, status.Errorf(codes.Internal, "failed to purge team %q from policies: %s", req.Id, err.Error())
	}

	return &teams.DeleteTeamResp{
		Team: fromStorage(team),
	}, nil
}

// UpdateTeam updates a team in the db via post
func (s *Server) UpdateTeam(ctx context.Context, req *teams.UpdateTeamReq) (*teams.UpdateTeamResp, error) {
	ctx, cancel := context.WithCancel(ctx)
	defer cancel()

	team, err := s.service.Storage.EditTeam(ctx, req.Id, req.Name, req.Projects)
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
func (s *Server) AddTeamMembers(ctx context.Context,
	req *teams.AddTeamMembersReq) (*teams.AddTeamMembersResp, error) {

	if len(req.UserIds) == 0 {
		return nil, status.Error(codes.InvalidArgument, "missing user IDs")
	}

	// verify team exists and isn't filtered out by the project filter
	_, err := s.service.Storage.GetTeam(ctx, req.Id)
	if err != nil {
		return nil, service.ParseStorageError(err, req.Id, "team")
	}

	_, err = s.service.Storage.AddUsers(ctx, req.Id, req.UserIds)
	if err != nil && err != storage.ErrConflict {
		return nil, service.ParseStorageError(err, req.Id, "user")
	}

	// TODO (tc): Get the updated set of user membership for the team since
	// that is not what is returned from AddUsers for some reason
	// (can refactor on V1 deprecation).
	userIDs, err := s.service.Storage.GetUserIDsForTeam(ctx, req.Id)
	if err != nil {
		return nil, service.ParseStorageError(err, req.Id, "team")
	}

	return &teams.AddTeamMembersResp{
		UserIds: userIDs,
	}, nil
}

// RemoveTeamMembers disassociates an array of members with an existing team.
func (s *Server) RemoveTeamMembers(ctx context.Context,
	req *teams.RemoveTeamMembersReq) (*teams.RemoveTeamMembersResp, error) {

	// verify team exists and isn't filtered out by the project filter
	_, err := s.service.Storage.GetTeam(ctx, req.Id)
	if err != nil {
		return nil, service.ParseStorageError(err, req.Id, "team")
	}

	_, err = s.service.Storage.RemoveUsers(ctx, req.Id, req.UserIds)
	if err != nil {
		return nil, service.ParseStorageError(err, req.Id, "team")
	}

	// TODO do this TODO
	// TODO (tc): Get the updated set of user membership for the team since
	// that is not what is returned from RemoveUsers for some reason
	// (can refactor on V1 deprecation).
	userIDs, err := s.service.Storage.GetUserIDsForTeam(ctx, req.Id)
	if err != nil {
		return nil, service.ParseStorageError(err, req.Id, "team")
	}

	return &teams.RemoveTeamMembersResp{
		UserIds: userIDs,
	}, nil
}

// GetTeamsForMember fetches a list of a members's associated teams
func (s *Server) GetTeamsForMember(
	ctx context.Context,
	req *teams.GetTeamsForMemberReq,
) (*teams.GetTeamsForMemberResp, error) {

	teamList, err := s.service.Storage.GetTeamsForUser(ctx, req.UserId)
	if err != nil {
		return nil, service.ParseStorageError(err, req.UserId, "user")
	}

	return &teams.GetTeamsForMemberResp{
		Teams: fromStorageToList(teamList),
	}, nil
}

// GetTeamMembership fetches a list of member ids associated with a team
func (s *Server) GetTeamMembership(ctx context.Context,
	req *teams.GetTeamMembershipReq) (*teams.GetTeamMembershipResp, error) {

	// verify team exists and isn't filtered out by the project filter
	_, err := s.service.Storage.GetTeam(ctx, req.Id)
	if err != nil {
		return nil, service.ParseStorageError(err, req.Id, "team")
	}

	userIDs, err := s.service.Storage.GetUserIDsForTeam(ctx, req.Id)
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
func (s *Server) PurgeUserMembership(ctx context.Context,
	req *teams.PurgeUserMembershipReq) (*teams.PurgeUserMembershipResp, error) {

	if req.UserId == "" {
		return nil, status.Error(codes.InvalidArgument, "invalid userId")
	}

	teamIDs, err := s.service.Storage.PurgeUserMembership(ctx, req.UserId)
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
