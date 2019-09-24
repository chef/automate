package teams

import (
	"context"

	teams "github.com/chef/automate/api/interservice/teams/v2"

	// Teams Service Gateway Requests/Response/Service definitions
	gwcommon "github.com/chef/automate/components/automate-gateway/api/iam/v2beta/common"
	gwreq "github.com/chef/automate/components/automate-gateway/api/iam/v2beta/request"
	gwres "github.com/chef/automate/components/automate-gateway/api/iam/v2beta/response"
)

type Server struct {
	client teams.TeamsV2Client
}

func NewServer(client teams.TeamsV2Client) *Server {
	return &Server{
		client: client,
	}
}

// ListTeams fetches an array of existing teams
func (a *Server) ListTeams(ctx context.Context, _ *gwreq.ListTeamsReq) (*gwres.ListTeamsResp, error) {
	req := &teams.ListTeamsReq{}

	res, err := a.client.ListTeams(ctx, req)
	if err != nil {
		return nil, err
	}

	return &gwres.ListTeamsResp{
		Teams: fromUpstreamTeams(res.Teams),
	}, nil
}

// GetTeam fetches a single team by ID
func (a *Server) GetTeam(ctx context.Context, r *gwreq.GetTeamReq) (*gwres.GetTeamResp, error) {
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

// CreateTeam creates a team upstream
func (a *Server) CreateTeam(ctx context.Context, r *gwreq.CreateTeamReq) (*gwres.CreateTeamResp, error) {
	req := &teams.CreateTeamReq{
		Id:       r.Id,
		Name:     r.Name,
		Projects: r.Projects,
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
func (a *Server) UpdateTeam(ctx context.Context, r *gwreq.UpdateTeamReq) (*gwres.UpdateTeamResp, error) {
	req := &teams.UpdateTeamReq{
		Id:       r.Id,
		Name:     r.Name,
		Projects: r.Projects,
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
func (a *Server) DeleteTeam(ctx context.Context, r *gwreq.DeleteTeamReq) (*gwres.DeleteTeamResp, error) {
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

// GetTeamMembership returns an array of userIDs for a team
func (a *Server) GetTeamMembership(ctx context.Context,
	r *gwreq.GetTeamMembershipReq) (*gwres.GetTeamMembershipResp, error) {

	res, err := a.client.GetTeamMembership(ctx, (*teams.GetTeamMembershipReq)(r))
	return (*gwres.GetTeamMembershipResp)(res), err
}

// AddTeamMembers adds the specified userIDs to the existing membership of a team
func (a *Server) AddTeamMembers(ctx context.Context,
	r *gwreq.AddTeamMembersReq) (*gwres.AddTeamMembersResp, error) {

	req := &teams.AddTeamMembersReq{
		Id:      r.Id,
		UserIds: r.UserIds,
	}

	res, err := a.client.AddTeamMembers(ctx, req)
	if err != nil {
		return nil, err
	}
	return &gwres.AddTeamMembersResp{
		UserIds: res.UserIds,
	}, nil
}

// RemoveTeamMembers deletes the specified userIDs from the existing membership of a team
func (a *Server) RemoveTeamMembers(ctx context.Context,
	r *gwreq.RemoveTeamMembersReq) (*gwres.RemoveTeamMembersResp, error) {

	req := &teams.RemoveTeamMembersReq{
		Id:      r.Id,
		UserIds: r.UserIds,
	}

	res, err := a.client.RemoveTeamMembers(ctx, req)
	if err != nil {
		return nil, err
	}
	return &gwres.RemoveTeamMembersResp{
		UserIds: res.UserIds,
	}, nil
}

// GetTeamsForMember returns all the teams for a specific user
func (a *Server) GetTeamsForMember(ctx context.Context,
	r *gwreq.GetTeamsForMemberReq) (*gwres.GetTeamsForMemberResp, error) {

	res, err := a.client.GetTeamsForMember(ctx, &teams.GetTeamsForMemberReq{UserId: r.Id})
	if err != nil {
		return nil, err
	}

	return &gwres.GetTeamsForMemberResp{
		Teams: fromUpstreamTeams(res.Teams),
	}, nil
}

func (a *Server) ApplyV2DataMigrations(ctx context.Context,
	r *gwreq.ApplyV2DataMigrationsReq) (*gwres.ApplyV2DataMigrationsResp, error) {

	_, err := a.client.UpgradeToV2(ctx, &teams.UpgradeToV2Req{})
	if err != nil {
		return nil, err
	}

	return &gwres.ApplyV2DataMigrationsResp{}, nil
}

func fromUpstreamTeam(t *teams.Team) *gwcommon.Team {
	return &gwcommon.Team{
		Id:       t.GetId(),
		Name:     t.GetName(),
		Projects: t.GetProjects(),
	}
}

func fromUpstreamTeams(teams []*teams.Team) []*gwcommon.Team {
	ts := []*gwcommon.Team{}
	for _, t := range teams {
		ts = append(ts, fromUpstreamTeam(t))
	}
	return ts
}
