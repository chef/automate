package server

import (
	"context"
	"sort"

	chef "github.com/go-chef/chef"

	"github.com/chef/automate/api/interservice/infra_proxy/request"
	"github.com/chef/automate/api/interservice/infra_proxy/response"
)

// GetClients get clients list
func (s *Server) GetClients(ctx context.Context, req *request.Clients) (*response.Clients, error) {
	c, err := s.createClient(ctx, req.OrgId, req.ServerId)
	if err != nil {
		return nil, err
	}

	clients, err := c.client.Clients.List()
	if err != nil {
		return nil, ParseAPIError(err, "", "client")
	}

	return &response.Clients{
		Clients: fromAPIToListClients(clients),
	}, nil
}

// GetClient get client
func (s *Server) GetClient(ctx context.Context, req *request.Client) (*response.Client, error) {
	c, err := s.createClient(ctx, req.OrgId, req.ServerId)
	if err != nil {
		return nil, err
	}

	ic, err := c.client.Clients.Get(req.Name)
	if err != nil {
		return nil, ParseAPIError(err, req.Name, "client")
	}

	return &response.Client{
		Name:       ic.Name,
		ClientName: ic.ClientName,
		OrgName:    ic.OrgName,
		Validator:  ic.Validator,
		JsonClass:  ic.JsonClass,
		ChefType:   ic.ChefType,
	}, nil

}

// fromAPIToListClients a response.Clients from a struct of ClientList
func fromAPIToListClients(al chef.ApiClientListResult) []*response.ClientListItem {
	cl := make([]*response.ClientListItem, len(al))

	index := 0
	for c := range al {
		cl[index] = &response.ClientListItem{
			Name: c,
		}
		index++
	}

	sort.Slice(cl, func(i, j int) bool {
		return cl[i].Name < cl[j].Name
	})

	return cl
}
