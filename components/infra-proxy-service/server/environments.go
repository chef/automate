package server

import (
	"context"
	"encoding/json"

	chef "github.com/go-chef/chef"
	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/status"

	"github.com/chef/automate/api/interservice/infra_proxy/request"
	"github.com/chef/automate/api/interservice/infra_proxy/response"
	"github.com/chef/automate/components/infra-proxy-service/validation"
)

// GetEnvironments gets environments list
func (s *Server) GetEnvironments(ctx context.Context, req *request.Environments) (*response.Environments, error) {
	c, err := s.createClient(ctx, req.OrgId, req.ServerId)
	if err != nil {
		return nil, err
	}

	res, err := c.SearchObjectsWithDefaults("environment", req.SearchQuery, nil)
	if err != nil {
		return nil, ParseAPIError(err)
	}

	return &response.Environments{
		Environments: fromAPIToListEnvironments(res.Rows),
		Page:         int32(res.Start),
		Total:        int32(res.Total),
	}, nil
}

// GetEnvironment gets the environment details
func (s *Server) GetEnvironment(ctx context.Context, req *request.Environment) (*response.Environment, error) {
	err := validation.New(validation.Options{
		Target:  "environment",
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
	err := validation.New(validation.Options{
		Target:  "environment",
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

	cookbooks := req.CookbookVersions
	if cookbooks == nil {
		cookbooks = map[string]string{}
	}

	defaultAttributes, err := StructToJSON(req.DefaultAttributes)
	if err != nil {
		return nil, status.Error(codes.Internal, err.Error())
	}

	overrideAttributes, err := StructToJSON(req.OverrideAttributes)
	if err != nil {
		return nil, status.Error(codes.Internal, err.Error())
	}

	_, err = c.client.Environments.Create(&chef.Environment{
		Name:               req.Name,
		Description:        req.Description,
		DefaultAttributes:  defaultAttributes,
		OverrideAttributes: overrideAttributes,
		CookbookVersions:   cookbooks,
		JsonClass:          req.JsonClass,
	})

	if err != nil {
		return nil, ParseAPIError(err)
	}

	environment, err := c.client.Environments.Get(req.Name)
	if err != nil {
		return nil, ParseAPIError(err)
	}

	return fromAPIToEnvironmentResponse(environment)
}

// DeleteEnvironment deletes the environment
func (s *Server) DeleteEnvironment(ctx context.Context, req *request.Environment) (*response.Environment, error) {
	err := validation.New(validation.Options{
		Target:  "environment",
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

	environment, err := c.client.Environments.Delete(req.Name)
	if err != nil {
		return nil, ParseAPIError(err)
	}

	return fromAPIToEnvironmentResponse(environment)
}

// UpdateEnvironment updates the environment attributes
func (s *Server) UpdateEnvironment(ctx context.Context, req *request.UpdateEnvironment) (*response.Environment, error) {
	err := validation.New(validation.Options{
		Target:  "environment",
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

	cookbooks := req.CookbookVersions
	if cookbooks == nil {
		cookbooks = map[string]string{}
	}

	defaultAttributes, err := StructToJSON(req.DefaultAttributes)
	if err != nil {
		return nil, status.Error(codes.Internal, err.Error())
	}

	overrideAttributes, err := StructToJSON(req.OverrideAttributes)
	if err != nil {
		return nil, status.Error(codes.Internal, err.Error())
	}

	environment, err := c.client.Environments.Put(&chef.Environment{
		Name:               req.Name,
		Description:        req.Description,
		DefaultAttributes:  defaultAttributes,
		OverrideAttributes: overrideAttributes,
		CookbookVersions:   cookbooks,
		JsonClass:          req.JsonClass,
	})

	if err != nil {
		return nil, ParseAPIError(err)
	}

	return fromAPIToEnvironmentResponse(environment)
}

// GetEnvironmentRecipes get environment based recipes list
func (s *Server) GetEnvironmentRecipes(ctx context.Context, req *request.Environment) (*response.EnvironmentRecipesList, error) {
	err := validation.New(validation.Options{
		Target:          "environment",
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

	recipes, err := c.client.Environments.ListRecipes(req.Name)
	if err != nil {
		return nil, ParseAPIError(err)
	}

	return &response.EnvironmentRecipesList{
		Recipes: recipes,
	}, nil
}

// fromAPIToListEnvironments a response.Environments from a struct of Environments
func fromAPIToListEnvironments(al []interface{}) []*response.EnvironmentListItem {
	cl := make([]*response.EnvironmentListItem, len(al))
	for index, e := range al {
		cl[index] = &response.EnvironmentListItem{
			Name:        SafeStringFromMap(e.(map[string]interface{}), "name"),
			Description: SafeStringFromMap(e.(map[string]interface{}), "description"),
		}
	}

	return cl
}

// fromAPIToEnvironmentResponse a response.Environment from a chef Environment
func fromAPIToEnvironmentResponse(environment *chef.Environment) (*response.Environment, error) {
	defaultAttributes, err := json.Marshal(environment.DefaultAttributes)
	if err != nil {
		return nil, status.Error(codes.Internal, err.Error())
	}

	overrideAttributes, err := json.Marshal(environment.OverrideAttributes)
	if err != nil {
		return nil, status.Error(codes.Internal, err.Error())
	}

	return &response.Environment{
		Name:               environment.Name,
		ChefType:           environment.ChefType,
		Description:        environment.Description,
		CookbookVersions:   environment.CookbookVersions,
		JsonClass:          environment.JsonClass,
		DefaultAttributes:  string(defaultAttributes),
		OverrideAttributes: string(overrideAttributes),
	}, nil
}
