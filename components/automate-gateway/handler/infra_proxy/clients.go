package infra_proxy

import (
	"context"

	gwreq "github.com/chef/automate/api/external/infra_proxy/request"
	gwres "github.com/chef/automate/api/external/infra_proxy/response"
	infra_req "github.com/chef/automate/api/interservice/infra_proxy/request"
	infra_res "github.com/chef/automate/api/interservice/infra_proxy/response"
)

// GetClients fetches an array of existing clients
func (a *InfraProxyServer) GetClients(ctx context.Context, r *gwreq.Clients) (*gwres.Clients, error) {
	req := &infra_req.Clients{
		OrgId:    r.OrgId,
		ServerId: r.ServerId,
	}
	res, err := a.client.GetClients(ctx, req)
	if err != nil {
		return nil, err
	}

	return &gwres.Clients{
		Clients: fromUpstreamClients(res.Clients),
	}, nil
}

// GetClient fetches an infra client details
func (a *InfraProxyServer) GetClient(ctx context.Context, r *gwreq.Client) (*gwres.Client, error) {
	req := &infra_req.Client{
		OrgId:    r.OrgId,
		ServerId: r.ServerId,
		Name:     r.Name,
	}
	res, err := a.client.GetClient(ctx, req)
	if err != nil {
		return nil, err
	}

	return &gwres.Client{
		Name:       res.GetName(),
		ClientName: res.GetClientName(),
		OrgName:    res.GetOrgName(),
		Validator:  res.GetValidator(),
		JsonClass:  res.GetJsonClass(),
		ChefType:   res.GetChefType(),
	}, nil
}

func fromUpstreamClients(clients []*infra_res.ClientListItem) []*gwres.ClientListItem {
	ts := make([]*gwres.ClientListItem, len(clients))

	for i, c := range clients {
		ts[i] = &gwres.ClientListItem{
			Name: c.GetName(),
		}
	}

	return ts
}
