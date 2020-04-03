package infra_proxy

import (
	"context"

	gwreq "github.com/chef/automate/api/external/infra_proxy/request"
	gwres "github.com/chef/automate/api/external/infra_proxy/response"
	infra_req "github.com/chef/automate/api/interservice/infra_proxy/request"
	infra_res "github.com/chef/automate/api/interservice/infra_proxy/response"
)

// GetRoles fetches an array of existing roles
func (a *InfraProxyServer) GetRoles(ctx context.Context, r *gwreq.Roles) (*gwres.Roles, error) {
	req := &infra_req.Roles{
		OrgId: r.OrgId,
	}
	res, err := a.client.GetRoles(ctx, req)
	if err != nil {
		return nil, err
	}

	return &gwres.Roles{
		Roles: fromUpstreamRoles(res.Roles),
	}, nil
}

// GetRole fetch a infra role
func (a *InfraProxyServer) GetRole(ctx context.Context, r *gwreq.Role) (*gwres.Role, error) {
	req := &infra_req.Role{
		OrgId: r.OrgId,
		Name:  r.Name,
	}
	res, err := a.client.GetRole(ctx, req)
	if err != nil {
		return nil, err
	}

	return &gwres.Role{
		Name:               res.GetName(),
		ChefType:           res.GetChefType(),
		Description:        res.GetDescription(),
		DefaultAttributes:  res.GetDefaultAttributes(),
		OverrideAttributes: res.GetOverrideAttributes(),
		RunList:            res.GetRunList(),
		ExpandedRunList:    fromUpstreamExpandedRunList(res.GetExpandedRunList()),
		JsonClass:          res.GetJsonClass(),
	}, nil
}

func fromUpstreamRoles(roles []*infra_res.RoleListItem) []*gwres.RoleListItem {
	ts := make([]*gwres.RoleListItem, len(roles))

	for i, c := range roles {
		ts[i] = &gwres.RoleListItem{
			Name:         c.GetName(),
			Description:  c.GetDescription(),
			Environments: c.GetEnvironments(),
		}
	}

	return ts
}

func fromUpstreamExpandedRunList(expRunList []*infra_res.ExpandedRunList) []*gwres.ExpandedRunList {
	exp := make([]*gwres.ExpandedRunList, len(expRunList))

	for i, e := range expRunList {
		exp[i] = &gwres.ExpandedRunList{
			Id:      e.GetId(),
			RunList: fromUpsteamRunList(e.GetRunList()),
		}
	}

	return exp
}

func fromUpsteamRunList(runList []*infra_res.RunList) []*gwres.RunList {
	resRunList := make([]*gwres.RunList, len(runList))

	for i, item := range runList {
		resRunListItem := gwres.RunList{
			Type:     item.GetType(),
			Name:     item.GetName(),
			Version:  item.GetVersion(),
			Children: fromUpsteamRunList(item.GetChildren()),
		}
		resRunList[i] = &resRunListItem
	}

	return resRunList
}
