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
		Name:               res.GetName(),
		ChefType:           res.GetChefType(),
		Description:        res.GetDescription(),
		DefaultAttributes:  res.GetDefaultAttributes(),
		OverrideAttributes: res.GetOverrideAttributes(),
		RunList:            res.GetRunList(),
		JsonClass:          res.GetJsonClass(),
	}, nil
}

// GetRoles fetches an array of existing roles
func (a *InfraProxyServer) GetRoles(ctx context.Context, r *gwreq.Roles) (*gwres.Roles, error) {
	req := &infra_req.Roles{
		OrgId:    r.OrgId,
		ServerId: r.ServerId,
		SearchQuery: &infra_req.SearchQuery{
			Q:       r.GetSearchQuery().GetQ(),
			Page:    r.GetSearchQuery().GetPage(),
			PerPage: r.GetSearchQuery().GetPerPage(),
		},
	}
	res, err := a.client.GetRoles(ctx, req)
	if err != nil {
		return nil, err
	}

	return &gwres.Roles{
		Roles: fromUpstreamRoles(res.Roles),
		Page:  res.GetPage(),
		Total: res.GetTotal(),
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
		JsonClass:          res.GetJsonClass(),
	}, nil
}

// GetRoleEnvironments gets the role environments
func (a *InfraProxyServer) GetRoleEnvironments(ctx context.Context, r *gwreq.Role) (*gwres.RoleEnvironments, error) {
	req := &infra_req.Role{
		OrgId:    r.OrgId,
		ServerId: r.ServerId,
		Name:     r.Name,
	}
	res, err := a.client.GetRoleEnvironments(ctx, req)
	if err != nil {
		return nil, err
	}

	return &gwres.RoleEnvironments{
		Environments: res.GetEnvironments(),
	}, nil
}

// GetRoleExpandedRunList gets the role expanded run-list
func (a *InfraProxyServer) GetRoleExpandedRunList(ctx context.Context, r *gwreq.ExpandedRunList) (*gwres.ExpandedRunList, error) {
	req := &infra_req.ExpandedRunList{
		OrgId:       r.OrgId,
		ServerId:    r.ServerId,
		Name:        r.Name,
		Environment: r.Environment,
	}
	res, err := a.client.GetRoleExpandedRunList(ctx, req)
	if err != nil {
		return nil, err
	}

	return &gwres.ExpandedRunList{
		Id:      res.GetId(),
		RunList: fromUpsteamRunList(res.GetRunList()),
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
		Name:               res.GetName(),
		ChefType:           res.GetChefType(),
		Description:        res.GetDescription(),
		DefaultAttributes:  res.GetDefaultAttributes(),
		OverrideAttributes: res.GetOverrideAttributes(),
		RunList:            res.GetRunList(),
		JsonClass:          res.GetJsonClass(),
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
		Name:               res.GetName(),
		ChefType:           res.GetChefType(),
		Description:        res.GetDescription(),
		DefaultAttributes:  res.GetDefaultAttributes(),
		OverrideAttributes: res.GetOverrideAttributes(),
		RunList:            res.GetRunList(),
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
