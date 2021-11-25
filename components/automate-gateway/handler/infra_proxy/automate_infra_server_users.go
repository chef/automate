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

//GetAutomateInfraServerUsersList: Fetches server users list from backend DB
func (c *InfraProxyServer) GetAutomateInfraServerOrgs(ctx context.Context, r *gwreq.AutomateInfraServerOrgs) (*gwres.AutomateInfraServerOrgs, error) {
	req := &infra_req.AutomateInfraServerOrgs{
		ServerId: r.ServerId,
		WebuiKey: r.WebuiKey,
	}
	res, err := c.client.GetAutomateInfraServerOrgs(ctx, req)
	if err != nil {
		return nil, err
	}
	return &gwres.AutomateInfraServerOrgs{
		Orgs: fromUpstreamAutomateInfraServerOrgs(res.Orgs),
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

func fromUpstreamAutomateInfraServerOrgs(ol []*infra_res.AutomateInfraServerOrg) []*gwres.AutomateInfraServerOrg {
	orgsList := make([]*response.AutomateInfraServerOrg, len(ol))

	for i, org := range ol {
		orgsList[i] = fromUpstreamAutomateInfraServerOrg(org)
	}

	return orgsList
}

func fromUpstreamAutomateInfraServerOrg(o *infra_res.AutomateInfraServerOrg) *gwres.AutomateInfraServerOrg {
	return &gwres.AutomateInfraServerOrg{
		Id:           o.Id,
		Name:         o.Name,
		AdminUser:    o.AdminUser,
		CredentialId: o.CredentialId,
		ServerId:     o.ServerId,
		Projects:     o.Projects,
	}
}
