package server

import (
	"context"
	"fmt"
	"sort"

	chef "github.com/go-chef/chef"

	"github.com/chef/automate/api/interservice/infra_proxy/request"
	"github.com/chef/automate/api/interservice/infra_proxy/response"
	"github.com/chef/automate/components/infra-proxy-service/validation"
)

// AccessKeyReq struct to add key.
type AccessKeyReq struct {
	Name           string `json:"name,omitempty"`
	PublicKey      string `json:"public_key,omitempty"`
	ExpirationDate string `json:"expiration_date,omitempty"`
	CreateKey      bool   `json:"create_key,omitempty"`
}

// GetClients get clients list
func (s *Server) GetClients(ctx context.Context, req *request.Clients) (*response.Clients, error) {
	c, err := s.createClient(ctx, req.OrgId, req.ServerId)
	if err != nil {
		return nil, err
	}

	clients, err := c.client.Clients.List()
	if err != nil {
		return nil, ParseAPIError(err)
	}

	return &response.Clients{
		Clients: fromAPIToListClients(clients),
	}, nil
}

// GetClient gets the client
func (s *Server) GetClient(ctx context.Context, req *request.Client) (*response.Client, error) {
	c, err := s.createClient(ctx, req.OrgId, req.ServerId)
	if err != nil {
		return nil, err
	}

	ic, err := c.client.Clients.Get(req.Name)
	if err != nil {
		return nil, ParseAPIError(err)
	}

	// Fetch associated default client key detail
	// Ignore if any error while fetching this.
	var clientKey response.ClientAccessKey
	chefKey, err := c.client.Clients.GetKey(req.Name, "default")
	if err == nil {
		clientKey = response.ClientAccessKey{
			Name:           chefKey.Name,
			ExpirationDate: chefKey.ExpirationDate,
			PublicKey:      chefKey.PublicKey,
		}
	}

	return &response.Client{
		Name:       ic.Name,
		ClientName: ic.ClientName,
		OrgName:    ic.OrgName,
		Validator:  ic.Validator,
		JsonClass:  ic.JsonClass,
		ChefType:   ic.ChefType,
		ClientKey:  &clientKey,
	}, nil

}

// CreateClient creates the client
func (s *Server) CreateClient(ctx context.Context, req *request.CreateClient) (*response.CreateClient, error) {
	err := validation.New(validation.Options{
		Target:  "client",
		Request: *req,
		Rules: validation.Rules{
			"OrgId":    []string{"required"},
			"ServerId": []string{"required"},
			"Name":     []string{"required"},
		},
	}).Validate()

	if err != nil {
		return nil, err
	}

	c, err := s.createClient(ctx, req.OrgId, req.ServerId)
	if err != nil {
		return nil, err
	}

	res, err := c.client.Clients.Create(chef.ApiNewClient{
		Name:      req.Name,
		Validator: req.Validator,
		CreateKey: req.CreateKey,
	})
	if err != nil {
		return nil, ParseAPIError(err)
	}

	return &response.CreateClient{
		Name: req.Name,
		ClientKey: &response.ClientKey{
			Name:           res.ChefKey.Name,
			PublicKey:      res.ChefKey.PublicKey,
			ExpirationDate: res.ChefKey.ExpirationDate,
			PrivateKey:     res.ChefKey.PrivateKey,
		},
	}, nil

}

// DeleteClient deletes the client
func (s *Server) DeleteClient(ctx context.Context, req *request.Client) (*response.Client, error) {
	err := validation.New(validation.Options{
		Target:          "client",
		Request:         *req,
		RequiredDefault: true,
	}).Validate()

	if err != nil {
		return nil, err
	}

	c, err := s.createClient(ctx, req.OrgId, req.ServerId)
	if err != nil {
		return nil, err
	}

	err = c.client.Clients.Delete(req.Name)
	if err != nil {
		return nil, ParseAPIError(err)
	}

	return &response.Client{
		Name: req.Name,
	}, nil

}

// ResetClientKey resets the client key
// Deletes the associated key pair and generates new key pair again, and then attached it to the client.
func (s *Server) ResetClientKey(ctx context.Context, req *request.ClientKey) (*response.ClientKey, error) {
	err := validation.New(validation.Options{
		Target:  "client",
		Request: *req,
		Rules: validation.Rules{
			"OrgId":    []string{"required"},
			"ServerId": []string{"required"},
			"Name":     []string{"required"},
		},
	}).Validate()

	if err != nil {
		return nil, err
	}

	c, err := s.createClient(ctx, req.OrgId, req.ServerId)
	if err != nil {
		return nil, err
	}

	key := req.Key

	if key == "" {
		key = "default"
	}

	// Deletes the existing key
	_, err = c.client.Clients.DeleteKey(req.Name, key)
	chefError, _ := chef.ChefError(err)
	if err != nil && chefError.StatusCode() != 404 {
		return nil, ParseAPIError(err)
	}

	// Add new key to existing client
	body, err := chef.JSONReader(AccessKeyReq{
		Name:           key,
		ExpirationDate: "infinity",
		CreateKey:      true,
	})

	var chefKey chef.ChefKey
	addReq, err := c.client.NewRequest("POST", fmt.Sprintf("clients/%s/keys", req.Name), body)
	res, err := c.client.Do(addReq, &chefKey)
	if res != nil {
		defer res.Body.Close()
	}

	if err != nil {
		return nil, ParseAPIError(err)
	}

	return &response.ClientKey{
		Name:       key,
		PrivateKey: chefKey.PrivateKey,
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
