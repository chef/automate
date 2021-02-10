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

// CreateClient creates an infra client
func (a *InfraProxyServer) CreateClient(ctx context.Context, r *gwreq.CreateClient) (*gwres.CreateClient, error) {
	req := &infra_req.CreateClient{
		OrgId:      r.OrgId,
		ServerId:   r.ServerId,
		Name:       r.Name,
		ClientName: r.ClientName,
		Validator:  r.Validator,
		CreateKey:  r.CreateKey,
	}
	res, err := a.client.CreateClient(ctx, req)
	if err != nil {
		return nil, err
	}

	return &gwres.CreateClient{
		Name: res.GetName(),
		ClientKey: &gwres.ClientKey{
			Name:           res.GetClientKey().GetName(),
			PublicKey:      res.GetClientKey().GetPublicKey(),
			ExpirationDate: res.GetClientKey().GetExpirationDate(),
			PrivateKey:     res.GetClientKey().GetPrivateKey(),
		},
	}, nil
}

// DeleteClient deletes an infra client
func (a *InfraProxyServer) DeleteClient(ctx context.Context, r *gwreq.Client) (*gwres.Client, error) {
	req := &infra_req.Client{
		OrgId:    r.OrgId,
		ServerId: r.ServerId,
		Name:     r.Name,
	}
	res, err := a.client.DeleteClient(ctx, req)
	if err != nil {
		return nil, err
	}

	return &gwres.Client{
		Name: res.GetName(),
	}, nil
}

// ResetClientKey resets an infra client
func (a *InfraProxyServer) ResetClientKey(ctx context.Context, r *gwreq.ClientKey) (*gwres.ClientKey, error) {
	req := &infra_req.ClientKey{
		OrgId:    r.OrgId,
		ServerId: r.ServerId,
		Name:     r.Name,
		KeyName:  r.KeyName,
	}
	res, err := a.client.ResetClientKey(ctx, req)
	if err != nil {
		return nil, err
	}

	return &gwres.ClientKey{
		Name:       res.GetName(),
		PrivateKey: res.GetPrivateKey(),
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
