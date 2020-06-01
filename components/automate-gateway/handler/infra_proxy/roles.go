package infra_proxy

import (
	"context"

	gwreq "github.com/chef/automate/api/external/infra_proxy/request"
	gwres "github.com/chef/automate/api/external/infra_proxy/response"
	infra_req "github.com/chef/automate/api/interservice/infra_proxy/request"
	infra_res "github.com/chef/automate/api/interservice/infra_proxy/response"
)

// CreateRole fetches an infra role details
func (a *InfraProxyServer) CreateRole(ctx context.Context, r *gwreq.CreateRole) (*gwres.Role, error) {
	req := &infra_req.CreateRole{
		OrgId:              r.OrgId,
		ServerId:           r.ServerId,
		Name:               r.Name,
		Description:        r.Description,
		DefaultAttributes:  r.DefaultAttributes,
		OverrideAttributes: r.OverrideAttributes,
		RunList:            r.RunList,
	}
	res, err := a.client.CreateRole(ctx, req)
	if err != nil {
		return nil, err
	}

	return &gwres.Role{
		Name: res.GetName(),
	}, nil
}

// GetRoles fetches an array of existing roles
func (a *InfraProxyServer) GetRoles(ctx context.Context, r *gwreq.Roles) (*gwres.Roles, error) {
	req := &infra_req.Roles{
		OrgId:    r.OrgId,
		ServerId: r.ServerId,
	}
	res, err := a.client.GetRoles(ctx, req)
	if err != nil {
		return nil, err
	}

	return &gwres.Roles{
		Roles: fromUpstreamRoles(res.Roles),
	}, nil
}

// GetRole fetches an infra role details
func (a *InfraProxyServer) GetRole(ctx context.Context, r *gwreq.Role) (*gwres.Role, error) {
	req := &infra_req.Role{
		OrgId:    r.OrgId,
		ServerId: r.ServerId,
		Name:     r.Name,
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
		ExpandedRunList:    GetUpstreamExpandedRunList(res.GetExpandedRunList()),
		JsonClass:          res.GetJsonClass(),
	}, nil
}

// DeleteRole deletes the role
func (a *InfraProxyServer) DeleteRole(ctx context.Context, r *gwreq.Role) (*gwres.Role, error) {
	req := &infra_req.Role{
		OrgId:    r.OrgId,
		ServerId: r.ServerId,
		Name:     r.Name,
	}
	res, err := a.client.DeleteRole(ctx, req)
	if err != nil {
		return nil, err
	}

	return &gwres.Role{
		Name: res.GetName(),
	}, nil
}

// UpdateRole updates an infra role
func (a *InfraProxyServer) UpdateRole(ctx context.Context, r *gwreq.UpdateRole) (*gwres.Role, error) {
	req := &infra_req.UpdateRole{
		OrgId:              r.OrgId,
		ServerId:           r.ServerId,
		Name:               r.Name,
		Description:        r.Description,
		DefaultAttributes:  r.DefaultAttributes,
		OverrideAttributes: r.OverrideAttributes,
		RunList:            r.RunList,
	}
	res, err := a.client.UpdateRole(ctx, req)
	if err != nil {
		return nil, err
	}

	return &gwres.Role{
		Name: res.GetName(),
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

// GetUpstreamExpandedRunList gets the expanded run-list from upstream API.
func GetUpstreamExpandedRunList(expRunList []*infra_res.ExpandedRunList) []*gwres.ExpandedRunList {
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
