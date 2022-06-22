package infra_proxy

import (
	"context"

	gwreq "github.com/chef/automate/api/external/infra_proxy/request"

	gwres "github.com/chef/automate/api/external/infra_proxy/response"
	infra_req "github.com/chef/automate/api/interservice/infra_proxy/request"
	infra_res "github.com/chef/automate/api/interservice/infra_proxy/response"
)

//GetUsersList: fetches a list of an existing users in organization
func (c *InfraProxyServer) GetOrgUsersList(ctx context.Context, r *gwreq.OrgUsers) (*gwres.OrgUsers, error) {
	req := &infra_req.OrgUsers{
		OrgId:    r.OrgId,
		ServerId: r.ServerId,
	}
	res, err := c.client.GetOrgUsersList(ctx, req)
	if err != nil {
		return nil, err
	}

	return &gwres.OrgUsers{
		Users: fromUpstreamUsers(res.Users),
	}, nil
}

//GetServerUsersList: fetches a list of an existing users in server
func (c *InfraProxyServer) GetServerUsersList(ctx context.Context, r *gwreq.ServerUsers) (*gwres.ServerUsers, error) {
	req := &infra_req.ServerUsers{
		ServerId:  r.ServerId,
		AdminName: r.GetAdminName(),
		AdminKey:  r.GetAdminKey(),
	}

	res, err := c.client.GetServerUsersList(ctx, req)
	if err != nil {
		return nil, err
	}

	return &gwres.ServerUsers{
		Users: fromUpstreamUsers(res.Users),
	}, nil
}

func fromUpstreamUsers(users []*infra_res.UsersListItem) []*gwres.UsersListItem {
	us := make([]*gwres.UsersListItem, len(users))

	for i, user := range users {
		us[i] = &gwres.UsersListItem{
			Username:  user.GetUsername(),
			FirstName: user.GetFirstName(),
			LastName:  user.GetLastName(),
			Email:     user.GetEmail(),
		}
	}

	return us
}
