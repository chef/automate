package infra_proxy

import (
	"context"

	gwreq "github.com/chef/automate/api/external/infra_proxy/request"

	gwres "github.com/chef/automate/api/external/infra_proxy/response"
	infra_req "github.com/chef/automate/api/interservice/infra_proxy/request"
	infra_res "github.com/chef/automate/api/interservice/infra_proxy/response"
)

//GetUsersList: fetches an array of existing users
func (c *InfraProxyServer) GetUsersList(ctx context.Context, r *gwreq.Users) (*gwres.Users, error) {
	req := &infra_req.Users{
		OrgId:    r.OrgId,
		ServerId: r.ServerId,
	}
	res, err := c.client.GetUsersList(ctx, req)
	if err != nil {
		return nil, err
	}

	return &gwres.Users{
		Users: fromUpstreamUsers(res.Users),
	}, nil
}

func fromUpstreamUsers(users []*infra_res.UsersListItem) []*gwres.UsersListItem {
	us := make([]*gwres.UsersListItem, len(users))

	for i, c := range users {
		us[i] = &gwres.UsersListItem{
			FirstName: c.GetFirstName(),
			LastName:  c.GetLastName(),
			Email:     c.GetEmail(),
		}
	}

	return us
}
