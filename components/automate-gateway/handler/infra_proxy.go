package handler

import (
	"context"

	gwreq "github.com/chef/automate/components/automate-gateway/api/infra_proxy/request"
	gwres "github.com/chef/automate/components/automate-gateway/api/infra_proxy/response"

	version "github.com/chef/automate/api/external/common/version"
	infra_req "github.com/chef/automate/api/interservice/infra_proxy/request"
	infra_res "github.com/chef/automate/api/interservice/infra_proxy/response"
	infra_proxy "github.com/chef/automate/api/interservice/infra_proxy/service"
)

// InfraProxyServer stores client
type InfraProxyServer struct {
	client infra_proxy.InfraProxyClient
}

// NewInfraProxyHandler initializes InfraProxyServer with client
func NewInfraProxyHandler(client infra_proxy.InfraProxyClient) *InfraProxyServer {
	return &InfraProxyServer{
		client: client,
	}
}

// GetVersion fetches the version of infra proxy service
func (a *InfraProxyServer) GetVersion(ctx context.Context, e *version.VersionInfoRequest) (*version.VersionInfo, error) {
	return a.client.GetVersion(ctx, e)
}

// GetServers fetches an array of existing servers
func (a *InfraProxyServer) GetServers(ctx context.Context, _ *gwreq.GetServers) (*gwres.GetServers, error) {
	req := &infra_req.GetServers{}

	res, err := a.client.GetServers(ctx, req)
	if err != nil {
		return nil, err
	}

	return &gwres.GetServers{
		Servers: fromUpstreamServers(res.Servers),
	}, nil
}

// GetServer fetches a single server by ID
func (a *InfraProxyServer) GetServer(ctx context.Context, r *gwreq.GetServer) (*gwres.GetServer, error) {
	req := &infra_req.GetServer{
		Id: r.Id,
	}
	res, err := a.client.GetServer(ctx, req)
	if err != nil {
		return nil, err
	}
	return &gwres.GetServer{
		Server: fromUpstreamServer(res.Server),
	}, nil
}

// CreateServer posts a server upstream
func (a *InfraProxyServer) CreateServer(ctx context.Context, r *gwreq.CreateServer) (*gwres.CreateServer, error) {
	req := &infra_req.CreateServer{
		Name:        r.Name,
		Description: r.Description,
		Fqdn:        r.Fqdn,
		IpAddress:   r.IpAddress,
	}
	res, err := a.client.CreateServer(ctx, req)
	if err != nil {
		return nil, err
	}
	return &gwres.CreateServer{
		Server: fromUpstreamServer(res.Server),
	}, nil
}

// UpdateServer updates a server upstream
func (a *InfraProxyServer) UpdateServer(ctx context.Context, r *gwreq.UpdateServer) (*gwres.UpdateServer, error) {
	req := &infra_req.UpdateServer{
		Id:          r.Id,
		Name:        r.Name,
		Description: r.Description,
		Fqdn:        r.Fqdn,
		IpAddress:   r.IpAddress,
	}
	res, err := a.client.UpdateServer(ctx, req)
	if err != nil {
		return nil, err
	}
	return &gwres.UpdateServer{
		Server: fromUpstreamServer(res.Server),
	}, nil
}

// DeleteServer deletes a server upstream
func (a *InfraProxyServer) DeleteServer(ctx context.Context, r *gwreq.DeleteServer) (*gwres.DeleteServer, error) {
	req := &infra_req.DeleteServer{
		Id: r.Id,
	}
	res, err := a.client.DeleteServer(ctx, req)
	if err != nil {
		return nil, err
	}
	return &gwres.DeleteServer{
		Server: fromUpstreamServer(res.Server),
	}, nil
}

func fromUpstreamServer(t *infra_res.Server) *gwres.Server {
	return &gwres.Server{
		Id:          t.GetId(),
		Name:        t.GetName(),
		Description: t.GetDescription(),
		Fqdn:        t.GetFqdn(),
		IpAddress:   t.GetIpAddress(),
	}
}

func fromUpstreamServers(servers []*infra_res.Server) []*gwres.Server {
	ts := []*gwres.Server{}
	for _, t := range servers {
		ts = append(ts, fromUpstreamServer(t))
	}
	return ts
}

// GetOrgs fetches an array of existing orgs
func (a *InfraProxyServer) GetOrgs(ctx context.Context, _ *gwreq.GetOrgs) (*gwres.GetOrgs, error) {
	req := &infra_req.GetOrgs{}
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

// CreateOrg posts a org upstream
func (a *InfraProxyServer) CreateOrg(ctx context.Context, r *gwreq.CreateOrg) (*gwres.CreateOrg, error) {
	req := &infra_req.CreateOrg{
		Name: r.Name,
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
		Id:   r.Id,
		Name: r.Name,
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
	}
}

func fromUpstreamOrgs(orgs []*infra_res.Org) []*gwres.Org {
	ts := []*gwres.Org{}
	for _, t := range orgs {
		ts = append(ts, fromUpstreamOrg(t))
	}
	return ts
}
