package v1

import (
	"context"
	"sort"

	chef "github.com/chef/go-chef"
	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/status"

	"github.com/chef/automate/api/interservice/infra_proxy/request"
	"github.com/chef/automate/api/interservice/infra_proxy/response"
)

// GetClients get clients list
func (s *Server) GetClients(ctx context.Context, req *request.Clients) (*response.Clients, error) {

	client, err := s.createClient(ctx, req.OrgId)
	if err != nil {
		return nil, status.Errorf(codes.InvalidArgument, "invalid org id: %s", err.Error())
	}

	clients, err := client.Clients.List()
	if err != nil {
		return nil, status.Error(codes.InvalidArgument, err.Error())
	}

	return &response.Clients{
		Clients: fromAPIToListClients(clients),
	}, nil
}

// GetClient get client
func (s *Server) GetClient(ctx context.Context, req *request.Client) (*response.Client, error) {
	client, err := s.createClient(ctx, req.OrgId)
	if err != nil {
		return nil, status.Errorf(codes.InvalidArgument, "invalid org id: %s", err.Error())
	}

	ic, err := client.Clients.Get(req.Name)
	if err != nil {
		return nil, status.Error(codes.InvalidArgument, err.Error())
	}

	return &response.Client{
		Name:        ic.Name,
		ClientName:  ic.ClientName,
		OrgName:     ic.OrgName,
		Admin:       ic.Admin,
		Validator:   ic.Validator,
		Certificate: ic.Certificate,
		PublicKey:   ic.PublicKey,
		PrivateKey:  ic.PrivateKey,
		Uri:         ic.Uri,
		JsonClass:   ic.JsonClass,
		ChefType:    ic.ChefType,
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
