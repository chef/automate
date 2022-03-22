package infra_proxy

import (
	"context"

	gwreq "github.com/chef/automate/api/external/infra_proxy/request"
	"github.com/chef/automate/api/external/infra_proxy/response"
	gwres "github.com/chef/automate/api/external/infra_proxy/response"

	infra_req "github.com/chef/automate/api/interservice/infra_proxy/request"
	infra_res "github.com/chef/automate/api/interservice/infra_proxy/response"
)

//GetAutomateInfraServerUsersList: Fetches server users list from backend DB
func (c *InfraProxyServer) GetAutomateInfraServerUsersList(ctx context.Context, r *gwreq.AutomateInfraServerUsers) (*gwres.AutomateInfraServerUsers, error) {
	req := &infra_req.AutomateInfraServerUsers{
		ServerId: r.ServerId,
	}
	res, err := c.client.GetAutomateInfraServerUsersList(ctx, req)
	if err != nil {
		return nil, err
	}
	return &gwres.AutomateInfraServerUsers{
		Users: fromUpstreamAutomateInfraServerUsers(res.Users),
	}, nil
}

//GetAutomateInfraOrgUsersList: Fetches org users list from backend DB
func (c *InfraProxyServer) GetAutomateInfraOrgUsersList(ctx context.Context, r *gwreq.AutomateInfraOrgUsers) (*gwres.AutomateInfraOrgUsers, error) {
	req := &infra_req.AutomateInfraOrgUsers{
		ServerId: r.ServerId,
		OrgId:    r.OrgId,
	}
	res, err := c.client.GetAutomateInfraOrgUsersList(ctx, req)
	if err != nil {
		return nil, err
	}
	return &gwres.AutomateInfraOrgUsers{
		Users: fromUpstreamAutomateInfraOrgUsers(res.Users),
	}, nil
}

func fromUpstreamAutomateInfraServerUsers(ul []*infra_res.AutomateInfraServerUsersListItem) []*gwres.AutomateInfraServerUsersListItem {
	usersList := make([]*response.AutomateInfraServerUsersListItem, len(ul))

	for i, user := range ul {
		usersList[i] = fromUpstreamAutomateInfraServerUser(user)
	}

	return usersList
}

func fromUpstreamAutomateInfraServerUser(u *infra_res.AutomateInfraServerUsersListItem) *gwres.AutomateInfraServerUsersListItem {
	return &gwres.AutomateInfraServerUsersListItem{
		Id:                  u.Id,
		ServerId:            u.ServerId,
		InfraServerUsername: u.InfraServerUsername,
		CredentialId:        u.CredentialId,
		Connector:           u.Connector,
		AutomateUserId:      u.AutomateUserId,
		IsServerAdmin:       u.IsServerAdmin,
	}
}

func fromUpstreamAutomateInfraOrgUsers(ul []*infra_res.AutomateInfraOrgUsersListItem) []*gwres.AutomateInfraOrgUsersListItem {
	usersList := make([]*gwres.AutomateInfraOrgUsersListItem, len(ul))

	for i, user := range ul {
		usersList[i] = fromUpstreamAutomateInfraOrgUser(user)
	}

	return usersList
}

func fromUpstreamAutomateInfraOrgUser(u *infra_res.AutomateInfraOrgUsersListItem) *gwres.AutomateInfraOrgUsersListItem {
	return &gwres.AutomateInfraOrgUsersListItem{
		UserId:              u.UserId,
		ServerId:            u.ServerId,
		OrgId:               u.OrgId,
		InfraServerUsername: u.InfraServerUsername,
		FirstName:           u.FirstName,
		LastName:            u.LastName,
		EmailId:             u.EmailId,
		MiddleName:          u.MiddleName,
		DisplayName:         u.DisplayName,
		Connector:           u.Connector,
		AutomateUserId:      u.AutomateUserId,
		IsAdmin:             u.IsAdmin,
	}
}
