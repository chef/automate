package infra_proxy

import (
	"context"
	gwreq "github.com/chef/automate/api/external/infra_proxy/request"
	gwres "github.com/chef/automate/api/external/infra_proxy/response"
	infra_req "github.com/chef/automate/api/interservice/infra_proxy/request"
	infra_res "github.com/chef/automate/api/interservice/infra_proxy/response"
	log "github.com/sirupsen/logrus"
)

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
		Id:        r.Id,
		Name:      r.Name,
		Fqdn:      r.Fqdn,
		IpAddress: r.IpAddress,
	}
	res, err := a.client.CreateServer(ctx, req)
	if err != nil {
		log.Warnf("Create server error:: %s", err.Error())
		return nil, err
	}
	return &gwres.CreateServer{
		Server: fromUpstreamServer(res.Server),
	}, nil
}

// UpdateServer updates a server upstream
func (a *InfraProxyServer) UpdateServer(ctx context.Context, r *gwreq.UpdateServer) (*gwres.UpdateServer, error) {
	req := &infra_req.UpdateServer{
		Id:        r.Id,
		Name:      r.Name,
		Fqdn:      r.Fqdn,
		IpAddress: r.IpAddress,
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

// GetServerStatus get status of server
func (a *InfraProxyServer) GetServerStatus(ctx context.Context, request *gwreq.GetServerStatus) (*gwres.GetServerStatus, error) {
	req := &infra_req.GetServerStatus{
		Fqdn: request.GetFqdn(),
	}

	res, err := a.client.GetServerStatus(ctx, req)
	if err != nil {
		return nil, err
	}
	return &gwres.GetServerStatus{
		Status:    res.Status,
		Upstreams: res.Upstreams,
		Keygen:    res.Keygen,
	}, nil
}

func fromUpstreamServer(t *infra_res.Server) *gwres.Server {
	return &gwres.Server{
		Id:        t.GetId(),
		Name:      t.GetName(),
		Fqdn:      t.GetFqdn(),
		IpAddress: t.GetIpAddress(),
		OrgsCount: t.GetOrgsCount(),
	}
}

func fromUpstreamServers(servers []*infra_res.Server) []*gwres.Server {
	ts := make([]*gwres.Server, len(servers))

	for i, t := range servers {
		ts[i] = fromUpstreamServer(t)
	}

	return ts
}
