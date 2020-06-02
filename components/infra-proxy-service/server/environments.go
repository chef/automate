package server

import (
	"context"
	"encoding/json"
	"sort"

	chef "github.com/go-chef/chef"
	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/status"

	"github.com/chef/automate/api/interservice/infra_proxy/request"
	"github.com/chef/automate/api/interservice/infra_proxy/response"
)

// GetEnvironments get environments list
func (s *Server) GetEnvironments(ctx context.Context, req *request.Environments) (*response.Environments, error) {
	c, err := s.createClient(ctx, req.OrgId, req.ServerId)
	if err != nil {
		return nil, err
	}

	environments, err := c.client.Environments.List()
	if err != nil {
		return nil, ParseAPIError(err)
	}

	return &response.Environments{
		Environments: fromAPIToListEnvironments(*environments),
	}, nil
}

// GetEnvironment gets the environment details
func (s *Server) GetEnvironment(ctx context.Context, req *request.Environment) (*response.Environment, error) {
	c, err := s.createClient(ctx, req.OrgId, req.ServerId)
	if err != nil {
		return nil, err
	}

	en, err := c.client.Environments.Get(req.Name)
	if err != nil {
		return nil, ParseAPIError(err)
	}

	defaultAttributes, err := json.Marshal(en.DefaultAttributes)
	if err != nil {
		return nil, status.Error(codes.Internal, err.Error())
	}

	overrideAttributes, err := json.Marshal(en.OverrideAttributes)
	if err != nil {
		return nil, status.Error(codes.Internal, err.Error())
	}

	return &response.Environment{
		Name:               en.Name,
		ChefType:           en.ChefType,
		Description:        en.Description,
		CookbookVersions:   en.CookbookVersions,
		JsonClass:          en.JsonClass,
		DefaultAttributes:  string(defaultAttributes),
		OverrideAttributes: string(overrideAttributes),
	}, nil

}

// CreateEnvironment creates the environment
func (s *Server) CreateEnvironment(ctx context.Context, req *request.CreateEnvironment) (*response.Environment, error) {
	c, err := s.createClient(ctx, req.OrgId, req.ServerId)
	if err != nil {
		return nil, err
	}

	if req.Name == "" {
		return nil, status.Error(codes.InvalidArgument, "must supply environment name")
	}

	cookbooks := req.CookbookVersions
	if len(cookbooks) == 0 {
		cookbooks = map[string]string{}
	}

	var defAtt interface{}
	var ovrAtt interface{}

	defIn := req.DefaultAttributes
	if defIn == "" {
		defIn = "{}"
	}

	overIn := req.OverrideAttributes
	if overIn == "" {
		overIn = "{}"
	}
	defaultAttributes := json.RawMessage(defIn)
	overrideAttributes := json.RawMessage(overIn)

	err = json.Unmarshal(defaultAttributes, &defAtt)
	if err != nil {
		return nil, status.Error(codes.Internal, err.Error())
	}

	err = json.Unmarshal(overrideAttributes, &ovrAtt)
	if err != nil {
		return nil, status.Error(codes.Internal, err.Error())
	}

	_, err = c.client.Environments.Create(&chef.Environment{
		Name:               req.Name,
		Description:        req.Description,
		DefaultAttributes:  defAtt,
		OverrideAttributes: ovrAtt,
		CookbookVersions:   cookbooks,
		JsonClass:          req.JsonClass,
	})

	if err != nil {
		return nil, ParseAPIError(err)
	}

	return &response.Environment{
		Name: req.Name,
	}, nil
}

// DeleteEnvironment deletes the environment
func (s *Server) DeleteEnvironment(ctx context.Context, req *request.Environment) (*response.Environment, error) {
	c, err := s.createClient(ctx, req.OrgId, req.ServerId)
	if err != nil {
		return nil, err
	}

	if req.Name == "" {
		return nil, status.Error(codes.InvalidArgument, "must supply environment name")
	}

	environment, err := c.client.Environments.Delete(req.Name)
	if err != nil {
		return nil, ParseAPIError(err)
	}

	return &response.Environment{
		Name: environment.Name,
	}, nil
}

// UpdateEnvironment updates the environment attributes
func (s *Server) UpdateEnvironment(ctx context.Context, req *request.UpdateEnvironment) (*response.Environment, error) {
	c, err := s.createClient(ctx, req.OrgId, req.ServerId)
	if err != nil {
		return nil, err
	}

	if req.Name == "" {
		return nil, status.Error(codes.InvalidArgument, "must supply environment name")
	}

	cookbooks := req.CookbookVersions
	if len(cookbooks) == 0 {
		cookbooks = map[string]string{}
	}

	var defAtt interface{}
	var ovrAtt interface{}

	defIn := req.DefaultAttributes
	if defIn == "" {
		defIn = "{}"
	}

	overIn := req.OverrideAttributes
	if overIn == "" {
		overIn = "{}"
	}
	defaultAttributes := json.RawMessage(defIn)
	overrideAttributes := json.RawMessage(overIn)

	err = json.Unmarshal(defaultAttributes, &defAtt)
	if err != nil {
		return nil, status.Error(codes.Internal, err.Error())
	}

	err = json.Unmarshal(overrideAttributes, &ovrAtt)
	if err != nil {
		return nil, status.Error(codes.Internal, err.Error())
	}

	_, err = c.client.Environments.Put(&chef.Environment{
		Name:               req.Name,
		Description:        req.Description,
		DefaultAttributes:  defAtt,
		OverrideAttributes: ovrAtt,
		CookbookVersions:   cookbooks,
		JsonClass:          req.JsonClass,
	})

	if err != nil {
		return nil, ParseAPIError(err)
	}

	return &response.Environment{
		Name: req.Name,
	}, nil
}

// fromAPIToListEnvironments a response.Environments from a struct of Environments
func fromAPIToListEnvironments(al chef.EnvironmentResult) []*response.EnvironmentListItem {
	cl := make([]*response.EnvironmentListItem, len(al))

	index := 0
	for c := range al {
		cl[index] = &response.EnvironmentListItem{
			Name: c,
		}
		index++
	}

	sort.Slice(cl, func(i, j int) bool {
		return cl[i].Name < cl[j].Name
	})

	return cl
}
