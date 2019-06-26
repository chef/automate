package v1

import (
	"context"
	"fmt"

	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/status"

	ver_api "github.com/chef/automate/api/external/common/version"
	authz "github.com/chef/automate/api/interservice/authz/common"
	teams "github.com/chef/automate/api/interservice/teams/v1"
	"github.com/chef/automate/components/teams-service/service"
	"github.com/chef/automate/components/teams-service/storage"
	uuid "github.com/chef/automate/lib/uuid4"
	"github.com/chef/automate/lib/version"
)

// TODO (tc): V1 is deprecated and will be removed a bit after GA of V2.
// We should only make bugfixes, etc in this file.

// Server is a V1 teams server
type Server struct {
	service *service.Service
}

// NewServer returns a V1 Teams server
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

// CreateTeam creates a new team
func (s *Server) CreateTeam(ctx context.Context, req *teams.CreateTeamReq) (*teams.CreateTeamResp, error) {
	ctx, cancel := context.WithCancel(ctx)
	defer cancel()

	if req.Name == "" {
		s.service.Logger.Debug("incomplete create team request: missing team name")
		return nil, status.Error(codes.InvalidArgument, "must supply team name")
	}

	if req.Description == "" {
		s.service.Logger.Debug("incomplete create team request: missing team description")
		return nil, status.Error(codes.InvalidArgument, "must supply team description")
	}

	var team storage.Team
	var err error
	if team, err = s.service.Storage.StoreTeam(ctx, req.Name, req.Description); err != nil {
		if err == storage.ErrConflict {
			return nil, status.Errorf(
				codes.AlreadyExists,
				"unable to create team: a team with name %q already exists.",
				req.Name)
		}
		return nil, status.Error(codes.Internal, err.Error())
	}

	return &teams.CreateTeamResp{
		Team: fromStorage(team),
	}, nil
}

// GetTeams returns a list of teams from the db
func (s *Server) GetTeams(ctx context.Context, req *teams.GetTeamsReq) (*teams.GetTeamsResp, error) {
	ctx, cancel := context.WithCancel(ctx)
	defer cancel()

	teamsList, err := s.service.Storage.GetTeams(ctx)
	if err != nil {
		return nil, status.Error(codes.Internal, err.Error())
	}

	return &teams.GetTeamsResp{
		Teams: fromStorageToList(teamsList),
	}, nil
}

// GetTeam takes an ID and returns a team object
func (s *Server) GetTeam(ctx context.Context, req *teams.GetTeamReq) (*teams.GetTeamResp, error) {
	ctx, cancel := context.WithCancel(ctx)
	defer cancel()

	UUID, err := uuid.FromString(req.Id)
	if err != nil {
		return nil, status.Error(codes.InvalidArgument, "invalid team id")
	}

	team, err := s.service.Storage.GetTeam(ctx, UUID)
	if err != nil {
		return nil, service.ParseStorageError(err, req.Id, "team")
	}

	return &teams.GetTeamResp{
		Team: fromStorage(team),
	}, nil
}

// DeleteTeam deletes a team from the db
func (s *Server) DeleteTeam(ctx context.Context, req *teams.DeleteTeamReq) (*teams.DeleteTeamResp, error) {
	ctx, cancel := context.WithCancel(ctx)
	defer cancel()

	UUID, err := uuid.FromString(req.Id)
	if err != nil {
		return nil, status.Error(codes.InvalidArgument, "invalid team id")
	}

	team, err := s.service.Storage.DeleteTeam(ctx, UUID)
	if err != nil {
		return nil, service.ParseStorageError(err, req.Id, "team")
	}

	teamSubject := "team:local:" + team.Name
	_, err = s.service.AuthzClient.PurgeSubjectFromPolicies(ctx, &authz.PurgeSubjectFromPoliciesReq{
		Subject: teamSubject,
	})
	if err != nil {
		s.service.Logger.Warnf("failed to purge subjects on team delete: %s", err.Error())
		return nil, status.Error(codes.Internal,
			fmt.Sprintf("the team named %s with id %s was successfully deleted but its "+
				"subject could not be purged from the policies: %s", team.Name, team.ID, err.Error()))
	}

	return &teams.DeleteTeamResp{
		Team: fromStorage(team),
	}, nil
}

// UpdateTeam updates a team in the db via post
func (s *Server) UpdateTeam(ctx context.Context, req *teams.UpdateTeamReq) (*teams.UpdateTeamResp, error) {
	ctx, cancel := context.WithCancel(ctx)
	defer cancel()

	if req.Id == "" {
		s.service.Logger.Debug("incomplete update team request: missing team id")
		return nil, status.Error(codes.InvalidArgument, "must supply team id")
	}
	if req.Name == "" {
		s.service.Logger.Debug("incomplete update team request: missing team name")
		return nil, status.Error(codes.InvalidArgument, "must supply team name")
	}
	if req.Description == "" {
		s.service.Logger.Debug("incomplete update team request: missing team description")
		return nil, status.Error(codes.InvalidArgument, "must supply team description")
	}

	id, err := uuid.FromString(req.Id)
	if err != nil {
		return nil, status.Error(codes.InvalidArgument, "invalid team id")
	}

	teamStruct := storage.Team{
		ID:          id,
		Name:        req.Name,
		Description: req.Description,
	}

	team, err := s.service.Storage.EditTeam(ctx, teamStruct)
	if err != nil {
		return nil, service.ParseStorageError(err, id, "team")
	}

	return &teams.UpdateTeamResp{
		Team: fromStorage(team),
	}, nil
}

// AddUsers associates an array of users with an existing team
func (s *Server) AddUsers(ctx context.Context, req *teams.AddUsersReq) (*teams.AddUsersResp, error) {
	if len(req.UserIds) == 0 {
		return nil, status.Error(codes.InvalidArgument, "missing user ids")
	}

	id, err := uuid.FromString(req.Id)
	if err != nil {
		return nil, status.Error(codes.InvalidArgument, "invalid team id")
	}

	team, err := s.service.Storage.AddUsers(ctx, id, req.UserIds)
	if err != nil && err != storage.ErrConflict {
		return nil, service.ParseStorageError(err, id, "user")
	}

	return &teams.AddUsersResp{
		Team: fromStorage(team),
	}, nil
}

// RemoveUsers disassociates an array of users with an existing team.
func (s *Server) RemoveUsers(ctx context.Context, req *teams.RemoveUsersReq) (*teams.RemoveUsersResp, error) {
	id, err := uuid.FromString(req.Id)
	if err != nil {
		return nil, status.Error(codes.InvalidArgument, "invalid team id")
	}

	team, err := s.service.Storage.RemoveUsers(ctx, id, req.UserIds)
	if err != nil {
		return nil, service.ParseStorageError(err, id, "team")
	}

	return &teams.RemoveUsersResp{
		Team: fromStorage(team),
	}, nil
}

// GetTeamsForUser fetches a list of a user's associated teams
func (s *Server) GetTeamsForUser(
	ctx context.Context,
	req *teams.GetTeamsForUserReq,
) (*teams.GetTeamsForUserResp, error) {

	if req.UserId == "" {
		return nil, status.Error(codes.InvalidArgument, "invalid userId")
	}

	teamList, err := s.service.Storage.GetTeamsForUser(ctx, req.UserId)
	if err != nil {
		return nil, service.ParseStorageError(err, req.UserId, "user")
	}

	return &teams.GetTeamsForUserResp{
		Teams: fromStorageToList(teamList),
	}, nil
}

// GetUsers fetches a list of user id's associated with a team
func (s *Server) GetUsers(ctx context.Context,
	req *teams.GetUsersReq) (*teams.GetUsersResp, error) {

	id, err := uuid.FromString(req.Id)
	if err != nil {
		return nil, status.Errorf(codes.InvalidArgument, "invalid team id: %q", id)
	}

	userIDs, err := s.service.Storage.GetUserIDsForTeam(ctx, id)
	if err != nil {
		return nil, service.ParseStorageError(err, req.Id, "team")
	}

	return &teams.GetUsersResp{
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

	teamUUIDs, err := s.service.Storage.PurgeUserMembership(ctx, req.UserId)
	if err != nil {
		return nil, service.ParseStorageError(err, req.UserId, "user")
	}

	teamIDs := make([]string, len(teamUUIDs))
	for i, teamUUID := range teamUUIDs {
		teamIDs[i] = teamUUID.String()
	}

	return &teams.PurgeUserMembershipResp{
		Ids: teamIDs,
	}, nil
}

// GetTeamByName looks up a team by its name string. This is intended as a helper for
// automate-deployment's boostrapping of an initial user and not something we currently
// have plans to wire into our HTTP API.
func (s *Server) GetTeamByName(ctx context.Context,
	req *teams.GetTeamByNameReq) (*teams.GetTeamByNameResp, error) {

	if req.Name == "" {
		s.service.Logger.Debug("incomplete GetTeamByName request: missing team name")
		return nil, status.Error(codes.InvalidArgument, "must supply team name")
	}

	team, err := s.service.Storage.GetTeamByName(ctx, req.Name)
	if err != nil {
		if err == storage.ErrNotFound {
			return nil, status.Errorf(codes.NotFound, "no team found with name %q", req.Name)
		}
		return nil, status.Errorf(codes.Internal,
			"unexpected error for GetTeamByName with team named %q", req.Name)
	}
	return &teams.GetTeamByNameResp{
		Team: fromStorage(team),
	}, nil
}

// Create a teams.Team from a storage.Team
func fromStorage(s storage.Team) *teams.Team {
	return &teams.Team{
		Id:          s.ID.String(),
		Name:        s.Name,
		Description: s.Description,
	}
}

// Create a teams.TeamsList from an array of storage.Team
func fromStorageToList(sl []storage.Team) []*teams.Team {
	tl := make([]*teams.Team, len(sl))

	for i, team := range sl {
		tl[i] = fromStorage(team)
	}

	return tl
}
