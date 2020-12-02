package infra_proxy

import (
	"context"

	gwreq "github.com/chef/automate/api/external/infra_proxy/request"
	gwres "github.com/chef/automate/api/external/infra_proxy/response"
	infra_req "github.com/chef/automate/api/interservice/infra_proxy/request"
	infra_res "github.com/chef/automate/api/interservice/infra_proxy/response"
)

// GetEnvironments fetches an array of existing environments
func (a *InfraProxyServer) GetEnvironments(ctx context.Context, r *gwreq.Environments) (*gwres.Environments, error) {
	req := &infra_req.Environments{
		OrgId:    r.OrgId,
		ServerId: r.ServerId,
	}
	res, err := a.client.GetEnvironments(ctx, req)
	if err != nil {
		return nil, err
	}

	return &gwres.Environments{
		Environments: fromUpstreamEnvironments(res.Environments),
	}, nil
}

// GetEnvironment fetches the environment details
func (a *InfraProxyServer) GetEnvironment(ctx context.Context, r *gwreq.Environment) (*gwres.Environment, error) {
	req := &infra_req.Environment{
		OrgId:    r.OrgId,
		ServerId: r.ServerId,
		Name:     r.Name,
	}
	res, err := a.client.GetEnvironment(ctx, req)
	if err != nil {
		return nil, err
	}

	return &gwres.Environment{
		Name:               res.GetName(),
		ChefType:           res.GetChefType(),
		Description:        res.GetDescription(),
		CookbookVersions:   res.GetCookbookVersions(),
		JsonClass:          res.GetJsonClass(),
		DefaultAttributes:  res.GetDefaultAttributes(),
		OverrideAttributes: res.GetOverrideAttributes(),
	}, nil
}

// CreateEnvironment creates the environment
func (a *InfraProxyServer) CreateEnvironment(ctx context.Context, r *gwreq.CreateEnvironment) (*gwres.Environment, error) {
	req := &infra_req.CreateEnvironment{
		OrgId:              r.OrgId,
		ServerId:           r.ServerId,
		Name:               r.Name,
		Description:        r.Description,
		CookbookVersions:   r.CookbookVersions,
		DefaultAttributes:  r.DefaultAttributes,
		OverrideAttributes: r.OverrideAttributes,
		JsonClass:          r.JsonClass,
	}
	res, err := a.client.CreateEnvironment(ctx, req)
	if err != nil {
		return nil, err
	}

	return &gwres.Environment{
		Name: res.GetName(),
	}, nil
}

// DeleteEnvironment deletes the environment
func (a *InfraProxyServer) DeleteEnvironment(ctx context.Context, r *gwreq.Environment) (*gwres.Environment, error) {
	req := &infra_req.Environment{
		OrgId:    r.OrgId,
		ServerId: r.ServerId,
		Name:     r.Name,
	}
	res, err := a.client.DeleteEnvironment(ctx, req)
	if err != nil {
		return nil, err
	}

	return &gwres.Environment{
		Name: res.GetName(),
	}, nil
}

// UpdateEnvironment updates the environment attributes
func (a *InfraProxyServer) UpdateEnvironment(ctx context.Context, r *gwreq.UpdateEnvironment) (*gwres.Environment, error) {
	req := &infra_req.UpdateEnvironment{
		OrgId:              r.OrgId,
		ServerId:           r.ServerId,
		Name:               r.Name,
		Description:        r.Description,
		CookbookVersions:   r.CookbookVersions,
		DefaultAttributes:  r.DefaultAttributes,
		OverrideAttributes: r.OverrideAttributes,
		JsonClass:          r.JsonClass,
	}
	res, err := a.client.UpdateEnvironment(ctx, req)
	if err != nil {
		return nil, err
	}

	return &gwres.Environment{
		Name: res.GetName(),
	}, nil
}

// GetEnvironmentRecipes fetches an array of environment based recipes
func (a *InfraProxyServer) GetEnvironmentRecipes(ctx context.Context, r *gwreq.Environment) (*gwres.EnvironmentRecipesList, error) {
	req := &infra_req.Environment{
		OrgId:    r.OrgId,
		ServerId: r.ServerId,
		Name:     r.Name,
	}
	res, err := a.client.GetEnvironmentRecipes(ctx, req)
	if err != nil {
		return nil, err
	}

	return &gwres.EnvironmentRecipesList{
		Name: res.GetName(),
	}, nil
}

func fromUpstreamEnvironments(environments []*infra_res.EnvironmentListItem) []*gwres.EnvironmentListItem {
	ts := make([]*gwres.EnvironmentListItem, len(environments))

	for i, e := range environments {
		ts[i] = &gwres.EnvironmentListItem{
			Name: e.GetName(),
		}
	}

	return ts
}
