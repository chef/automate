package infra_proxy

import (
	"context"

	gwreq "github.com/chef/automate/api/external/infra_proxy/request"
	gwres "github.com/chef/automate/api/external/infra_proxy/response"
	infra_req "github.com/chef/automate/api/interservice/infra_proxy/request"
	infra_res "github.com/chef/automate/api/interservice/infra_proxy/response"
)

// GetOrgs fetches an array of existing orgs
func (a *InfraProxyServer) GetOrgs(ctx context.Context, r *gwreq.GetOrgs) (*gwres.GetOrgs, error) {
	req := &infra_req.GetOrgs{
		ServerId: r.ServerId,
	}
	res, err := a.client.GetOrgs(ctx, req)
	if err != nil {
		return nil, err
	}

	return &gwres.GetOrgs{
		Orgs: fromUpstreamOrgs(res.Orgs),
	}, nil
}

// GetOrg fetches a single org by ID
func (a *InfraProxyServer) GetOrg(ctx context.Context, r *gwreq.GetOrg) (*gwres.GetOrg, error) {
	req := &infra_req.GetOrg{
		Id: r.Id,
	}
	res, err := a.client.GetOrg(ctx, req)
	if err != nil {
		return nil, err
	}
	return &gwres.GetOrg{
		Org: fromUpstreamOrg(res.Org),
	}, nil
}

// GetOrgByName fetches a single org by name
func (a *InfraProxyServer) GetOrgByName(ctx context.Context, r *gwreq.GetOrgByName) (*gwres.GetOrg, error) {
	req := &infra_req.GetOrgByName{
		Name:     r.Name,
		ServerId: r.ServerId,
	}
	res, err := a.client.GetOrgByName(ctx, req)
	if err != nil {
		return nil, err
	}
	return &gwres.GetOrg{
		Org: fromUpstreamOrg(res.Org),
	}, nil
}

// CreateOrg posts a org upstream
func (a *InfraProxyServer) CreateOrg(ctx context.Context, r *gwreq.CreateOrg) (*gwres.CreateOrg, error) {
	req := &infra_req.CreateOrg{
		Name:      r.Name,
		AdminUser: r.AdminUser,
		AdminKey:  r.AdminKey,
		ServerId:  r.ServerId,
		Projects:  r.Projects,
	}
	res, err := a.client.CreateOrg(ctx, req)
	if err != nil {
		return nil, err
	}
	return &gwres.CreateOrg{
		Org: fromUpstreamOrg(res.Org),
	}, nil
}

// UpdateOrg updates a org upstream
func (a *InfraProxyServer) UpdateOrg(ctx context.Context, r *gwreq.UpdateOrg) (*gwres.UpdateOrg, error) {
	req := &infra_req.UpdateOrg{
		Id:        r.Id,
		Name:      r.Name,
		AdminUser: r.AdminUser,
		AdminKey:  r.AdminKey,
		ServerId:  r.ServerId,
		Projects:  r.Projects,
	}
	res, err := a.client.UpdateOrg(ctx, req)
	if err != nil {
		return nil, err
	}
	return &gwres.UpdateOrg{
		Org: fromUpstreamOrg(res.Org),
	}, nil
}

// DeleteOrg deletes a org upstream
func (a *InfraProxyServer) DeleteOrg(ctx context.Context, r *gwreq.DeleteOrg) (*gwres.DeleteOrg, error) {
	req := &infra_req.DeleteOrg{
		Id: r.Id,
	}
	res, err := a.client.DeleteOrg(ctx, req)
	if err != nil {
		return nil, err
	}
	return &gwres.DeleteOrg{
		Org: fromUpstreamOrg(res.Org),
	}, nil
}

func fromUpstreamOrg(t *infra_res.Org) *gwres.Org {
	return &gwres.Org{
		Id:        t.GetId(),
		Name:      t.GetName(),
		AdminUser: t.GetAdminUser(),
		AdminKey:  t.GetAdminKey(),
		ServerId:  t.GetServerId(),
		Projects:  t.GetProjects(),
	}
}

func fromUpstreamOrgs(orgs []*infra_res.Org) []*gwres.Org {
	ts := make([]*gwres.Org, len(orgs))

	for i, org := range orgs {
		ts[i] = fromUpstreamOrg(org)
	}

	return ts
}
