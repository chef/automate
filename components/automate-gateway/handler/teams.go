package handler

import (
	"context"

	// Shared Response/Request definitions
	version "github.com/chef/automate/api/external/common/version"

	// Teams Service Requests/Response/Service definitions
	teams "github.com/chef/automate/api/interservice/teams/v1"

	// Teams Service Gateway Requests/Response/Service definitions
	gwreq "github.com/chef/automate/components/automate-gateway/api/auth/teams/request"
	gwres "github.com/chef/automate/components/automate-gateway/api/auth/teams/response"
)

type TeamsServer struct {
	client teams.TeamsV1Client
}

func NewTeamsServer(client teams.TeamsV1Client) *TeamsServer {
	return &TeamsServer{
		client: client,
	}
}

// GetVersion fetches the version of team service
func (a *TeamsServer) GetVersion(ctx context.Context, e *version.VersionInfoRequest) (*version.VersionInfo, error) {
	return a.client.GetVersion(ctx, e)
}

// GetTeams fetches an array of existing teams
func (a *TeamsServer) GetTeams(ctx context.Context, _ *gwreq.GetTeamsReq) (*gwres.Teams, error) {
	req := &teams.GetTeamsReq{}
	res, err := a.client.GetTeams(ctx, req)
	if err != nil {
		return nil, err
	}

	return &gwres.Teams{
		Teams: fromUpstreamTeams(res.Teams),
	}, nil
}

// GetTeam fetches a single team by ID
func (a *TeamsServer) GetTeam(ctx context.Context, r *gwreq.GetTeamReq) (*gwres.GetTeamResp, error) {
	req := &teams.GetTeamReq{
		Id: r.Id,
	}
	res, err := a.client.GetTeam(ctx, req)
	if err != nil {
		return nil, err
	}
	return &gwres.GetTeamResp{
		Team: fromUpstreamTeam(res.Team),
	}, nil
}

// CreateTeam posts a team upstream
func (a *TeamsServer) CreateTeam(ctx context.Context, r *gwreq.CreateTeamReq) (*gwres.CreateTeamResp, error) {
	req := &teams.CreateTeamReq{
		Name:        r.Name,
		Description: r.Description,
	}
	res, err := a.client.CreateTeam(ctx, req)
	if err != nil {
		return nil, err
	}
	return &gwres.CreateTeamResp{
		Team: fromUpstreamTeam(res.Team),
	}, nil
}

// UpdateTeam updates a team upstream
func (a *TeamsServer) UpdateTeam(ctx context.Context, r *gwreq.UpdateTeamReq) (*gwres.UpdateTeamResp, error) {
	req := &teams.UpdateTeamReq{
		Id:          r.Id,
		Name:        r.Name,
		Description: r.Description,
	}
	res, err := a.client.UpdateTeam(ctx, req)
	if err != nil {
		return nil, err
	}
	return &gwres.UpdateTeamResp{
		Team: fromUpstreamTeam(res.Team),
	}, nil
}

// DeleteTeam deletes a team upstream
func (a *TeamsServer) DeleteTeam(ctx context.Context, r *gwreq.DeleteTeamReq) (*gwres.DeleteTeamResp, error) {
	req := &teams.DeleteTeamReq{
		Id: r.Id,
	}
	res, err := a.client.DeleteTeam(ctx, req)
	if err != nil {
		return nil, err
	}
	return &gwres.DeleteTeamResp{
		Team: fromUpstreamTeam(res.Team),
	}, nil
}

// AddUsers puts an array of userIDs to a team upstream
func (a *TeamsServer) AddUsers(ctx context.Context, r *gwreq.AddUsersReq) (*gwres.AddUsersResp, error) {
	req := &teams.AddUsersReq{
		Id:      r.Id,
		UserIds: r.UserIds,
	}
	res, err := a.client.AddUsers(ctx, req)
	if err != nil {
		return nil, err
	}
	return &gwres.AddUsersResp{
		Team: fromUpstreamTeam(res.Team),
	}, nil
}

// GetUsers returns an array of userIDs for a team downstream
func (a *TeamsServer) GetUsers(ctx context.Context, r *gwreq.GetUsersReq) (*gwres.GetUsersResp, error) {
	res, err := a.client.GetUsers(ctx, (*teams.GetUsersReq)(r))
	return (*gwres.GetUsersResp)(res), err
}

// RemoveUsers deletes an array of userIDs from a team upstream
func (a *TeamsServer) RemoveUsers(ctx context.Context, r *gwreq.RemoveUsersReq) (*gwres.RemoveUsersResp, error) {
	req := &teams.RemoveUsersReq{
		Id:      r.Id,
		UserIds: r.UserIds,
	}
	res, err := a.client.RemoveUsers(ctx, req)
	if err != nil {
		return nil, err
	}
	return &gwres.RemoveUsersResp{
		Team: fromUpstreamTeam(res.Team),
	}, nil
}

// GetTeamsForUser returns all the teams for a specific user
func (a *TeamsServer) GetTeamsForUser(ctx context.Context,
	r *gwreq.GetTeamsForUserReq) (*gwres.GetTeamsForUserResp, error) {
	res, err := a.client.GetTeamsForUser(ctx, &teams.GetTeamsForUserReq{UserId: r.Id})
	if err != nil {
		return nil, err
	}
	return &gwres.GetTeamsForUserResp{Teams: fromUpstreamTeams(res.Teams)}, nil
}

func fromUpstreamTeam(t *teams.Team) *gwres.Team {
	return &gwres.Team{
		Id:          t.GetId(),
		Description: t.GetDescription(),
		Name:        t.GetName(),
	}
}

func fromUpstreamTeams(teams []*teams.Team) []*gwres.Team {
	ts := []*gwres.Team{}
	for _, t := range teams {
		ts = append(ts, fromUpstreamTeam(t))
	}
	return ts
}
