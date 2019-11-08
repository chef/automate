package handler

import (
	"context"

	gwreq "github.com/chef/automate/components/automate-gateway/api/infra_proxy/request"
	gwres "github.com/chef/automate/components/automate-gateway/api/infra_proxy/response"

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

func fromUpstreamServer(t *infra_res.Server) *gwres.Server {
	return &gwres.Server{
		Id:        t.GetId(),
		Name:      t.GetName(),
		Fqdn:      t.GetFqdn(),
		IpAddress: t.GetIpAddress(),
	}
}

func fromUpstreamServers(servers []*infra_res.Server) []*gwres.Server {
	ts := []*gwres.Server{}
	for _, t := range servers {
		ts = append(ts, fromUpstreamServer(t))
	}
	return ts
}
