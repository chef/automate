package server

import (
	"context"
	"encoding/json"
	"fmt"

	chef "github.com/go-chef/chef"
	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/status"

	"github.com/chef/automate/api/interservice/infra_proxy/request"
	"github.com/chef/automate/api/interservice/infra_proxy/response"
	"github.com/chef/automate/components/infra-proxy-service/validation"
)

// Environment represents the search based deserialized Environment type
type Environment struct {
	Name        string `json:"name"`
	Description string `json:"description"`
}

// EnvironmentResult list result from Search API
type EnvironmentResult struct {
	Total int            `json:"total"`
	Start int            `json:"start"`
	Rows  []*Environment `json:"rows"`
}

// GetEnvironments get environments list
func (s *Server) GetEnvironments(ctx context.Context, req *request.Environments) (*response.Environments, error) {
	c, err := s.createClient(ctx, req.OrgId, req.ServerId)
	if err != nil {
		return nil, err
	}

	res, err := c.SearchEnvironments(req.SearchQuery)
	if err != nil {
		return nil, ParseAPIError(err)
	}

	return &response.Environments{
		Environments: fromAPIToListEnvironments(res),
		Start:        int32(res.Start),
		Total:        int32(res.Total),
	}, nil
}

// SearchEnvironments gets environment list from Chef Infra Server search API.
func (c *ChefClient) SearchEnvironments(searchQuery *request.SearchQuery) (EnvironmentResult, error) {
	var result EnvironmentResult
	var searchAll bool
	inc := 1000
	var query chef.SearchQuery

	if searchQuery == nil || searchQuery.Q == "" {
		searchAll = true
		query = chef.SearchQuery{
			Index: "environment",
			Query: "*:*",
			Start: 0,
			Rows:  inc,
		}
	} else {
		perPage := int(searchQuery.GetRows())
		if perPage == 0 {
			perPage = 1000
		}

		query = chef.SearchQuery{
			Index: "environment",
			Query: searchQuery.GetQ(),
			Start: int(searchQuery.GetStart()),
			Rows:  perPage,
		}
	}

	fullURL := fmt.Sprintf("search/%s", query)
	newReq, err := c.client.NewRequest("GET", fullURL, nil)

	if err != nil {
		return result, ParseAPIError(err)
	}

	res, err := c.client.Do(newReq, &result)
	if err != nil {
		return result, ParseAPIError(err)
	}

	defer res.Body.Close() // nolint: errcheck

	if searchAll {
		var searchResult EnvironmentResult
		start := result.Start
		// the total rows available for this query across all pages
		total := result.Total
		for start+inc <= total {
			query.Start = query.Start + inc
			fullURL = fmt.Sprintf("search/%s", query)

			res1, err := c.client.Do(newReq, &searchResult)
			if err != nil {
				return result, ParseAPIError(err)
			}

			defer res1.Body.Close() // nolint: errcheck

			// add this page of results to the primary SearchResult instance
			result.Rows = append(result.Rows, searchResult.Rows...)
		}
	}
	return result, nil
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

	return &response.Environment{
		Name: req.Name,
	}, nil
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

	return &response.Environment{
		Name: environment.Name,
	}, nil
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

	_, err = c.client.Environments.Put(&chef.Environment{
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

	return &response.Environment{
		Name: req.Name,
	}, nil
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
func fromAPIToListEnvironments(al EnvironmentResult) []*response.EnvironmentListItem {
	cl := make([]*response.EnvironmentListItem, len(al.Rows))
	for index, e := range al.Rows {
		cl[index] = &response.EnvironmentListItem{
			Name:        e.Name,
			Description: e.Description,
		}
	}

	return cl
}
