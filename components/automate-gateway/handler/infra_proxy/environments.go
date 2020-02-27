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
		OrgId: r.OrgId,
	}
	res, err := a.client.GetEnvironments(ctx, req)
	if err != nil {
		return nil, err
	}

	return &gwres.Environments{
		Environments: fromUpstreamEnvironments(res.Environments),
	}, nil
}

// GetEnvironment fetch a infra role
func (a *InfraProxyServer) GetEnvironment(ctx context.Context, r *gwreq.Environment) (*gwres.Environment, error) {
	req := &infra_req.Environment{
		OrgId: r.OrgId,
		Name:  r.Name,
	}
	res, err := a.client.GetEnvironment(ctx, req)
	if err != nil {
		return nil, err
	}

	return &gwres.Environment{
		Name:             res.GetName(),
		ChefType:         res.GetChefType(),
		Description:      res.GetDescription(),
		CookbookVersions: res.GetCookbookVersions(),
		JsonClass:        res.GetJsonClass(),
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
