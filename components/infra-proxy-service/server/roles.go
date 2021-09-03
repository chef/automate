package server

import (
	"context"
	"encoding/json"
	"fmt"
	"reflect"

	chef "github.com/go-chef/chef"
	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/status"

	"github.com/chef/automate/api/interservice/infra_proxy/request"
	"github.com/chef/automate/api/interservice/infra_proxy/response"
	"github.com/chef/automate/components/infra-proxy-service/validation"
)

// RoleListResult role list result from Search API
type RoleListResult struct {
	Total int          `json:"total"`
	Start int          `json:"start"`
	Rows  []*chef.Role `json:"rows"`
}

type RunListCache map[string]map[string]bool

var runlistCache = RunListCache{}

// CreateRole creates the role
func (s *Server) CreateRole(ctx context.Context, req *request.CreateRole) (*response.Role, error) {
	err := validation.New(validation.Options{
		Target:  "role",
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

	runList := req.RunList
	if len(runList) == 0 {
		runList = []string{}
	}

	defaultAttributes, err := StructToJSON(req.DefaultAttributes)
	if err != nil {
		return nil, status.Error(codes.Internal, err.Error())
	}

	overrideAttributes, err := StructToJSON(req.OverrideAttributes)
	if err != nil {
		return nil, status.Error(codes.Internal, err.Error())
	}

	_, err = c.client.Roles.Create(
		&chef.Role{
			Name:               req.Name,
			Description:        req.Description,
			RunList:            runList,
			DefaultAttributes:  defaultAttributes,
			OverrideAttributes: overrideAttributes,
			EnvRunList:         chef.EnvRunList{},
		})

	if err != nil {
		return nil, ParseAPIError(err)
	}

	role, err := c.client.Roles.Get(req.Name)
	if err != nil {
		return nil, ParseAPIError(err)
	}

	return fromAPIToRoleResponse(role)
}

// SearchRoles gets roles list from Chef Infra Server search API.
func (c *ChefClient) SearchRoles(searchQuery *request.SearchQuery) (RoleListResult, error) {
	var result RoleListResult
	perPage := int(searchQuery.GetPerPage())
	if perPage == 0 {
		perPage = 1000
	}

	searchStr := searchQuery.GetQ()
	if searchStr == "" {
		searchStr = "*:*"
	}

	query := chef.SearchQuery{
		Index: "role",
		Query: searchStr,
		Start: int(searchQuery.GetPage()) * perPage,
		Rows:  perPage,
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

	if result.Start != 0 {
		result.Start = result.Start / perPage
	}

	return result, nil
}

// GetRoles gets roles list
func (s *Server) GetRoles(ctx context.Context, req *request.Roles) (*response.Roles, error) {
	err := validation.New(validation.Options{
		Target:  "role",
		Request: *req,
		Rules: validation.Rules{
			"OrgId":    []string{"required"},
			"ServerId": []string{"required"},
		},
	}).Validate()
	if err != nil {
		return nil, err
	}

	client, err := s.createClient(ctx, req.OrgId, req.ServerId)
	if err != nil {
		return nil, err
	}

	result, err := client.SearchRoles(req.SearchQuery)
	if err != nil {
		return nil, err
	}

	return &response.Roles{
		Roles: fromAPIToListRoles(result),
		Page:  int32(result.Start),
		Total: int32(result.Total),
	}, nil
}

// GetRole gets the role
func (s *Server) GetRole(ctx context.Context, req *request.Role) (*response.Role, error) {
	err := validation.New(validation.Options{
		Target:          "role",
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

	role, err := c.client.Roles.Get(req.Name)
	if err != nil {
		return nil, ParseAPIError(err)
	}

	return fromAPIToRoleResponse(role)
}

// GetRoleEnvironments fetches the role environments.
func (s *Server) GetRoleEnvironments(ctx context.Context, req *request.Role) (*response.RoleEnvironments, error) {
	err := validation.New(validation.Options{
		Target:          "role",
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

	res, err := c.client.Roles.GetEnvironments(req.Name)
	if err != nil {
		return nil, ParseAPIError(err)
	}

	return &response.RoleEnvironments{
		Environments: res,
	}, nil
}

// GetRoleExpandedRunList fetches the role run-list.
func (s *Server) GetRoleExpandedRunList(ctx context.Context, req *request.ExpandedRunList) (*response.ExpandedRunList, error) {
	err := validation.New(validation.Options{
		Target:          "role",
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

	envRunList, err := c.client.Roles.GetEnvironmentRunlist(req.Name, req.Environment)
	if err != nil {
		return nil, ParseAPIError(err)
	}

	// Fetches cookbooks to evaluate recipes version.
	cookbooks, err := c.client.Environments.ListCookbooks(req.Environment, "1")
	if err != nil {
		return nil, ParseAPIError(err)
	}

	runlist, err := ToResponseExpandedRunList(c, envRunList["run_list"], cookbooks, runlistCache)
	if err != nil {
		return nil, ParseAPIError(err)
	}

	// Reset the cache
	runlistCache = RunListCache{}

	return &response.ExpandedRunList{
		Id:      req.Environment,
		RunList: runlist,
	}, nil
}

// DeleteRole deletes the role
func (s *Server) DeleteRole(ctx context.Context, req *request.Role) (*response.Role, error) {
	err := validation.New(validation.Options{
		Target:          "role",
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

	err = c.client.Roles.Delete(req.Name)
	if err != nil {
		return nil, ParseAPIError(err)
	}

	return &response.Role{
		Name: req.Name,
	}, nil

}

// UpdateRole updates the role
func (s *Server) UpdateRole(ctx context.Context, req *request.UpdateRole) (*response.Role, error) {
	err := validation.New(validation.Options{
		Target:  "role",
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

	runList := req.RunList
	if len(runList) == 0 {
		runList = []string{}
	}

	defaultAttributes, err := StructToJSON(req.DefaultAttributes)
	if err != nil {
		return nil, status.Error(codes.Internal, err.Error())
	}

	overrideAttributes, err := StructToJSON(req.OverrideAttributes)
	if err != nil {
		return nil, status.Error(codes.Internal, err.Error())
	}

	role, err := c.client.Roles.Put(
		&chef.Role{
			Name:               req.Name,
			Description:        req.Description,
			RunList:            runList,
			DefaultAttributes:  defaultAttributes,
			OverrideAttributes: overrideAttributes,
			EnvRunList:         chef.EnvRunList{},
		})

	if err != nil {
		return nil, ParseAPIError(err)
	}

	return fromAPIToRoleResponse(role)
}

// fromAPIToListRoles a response.Roles from a struct of RoleList
func fromAPIToListRoles(result RoleListResult) []*response.RoleListItem {
	cl := make([]*response.RoleListItem, len(result.Rows))

	index := 0
	for _, role := range result.Rows {
		keys := reflect.ValueOf(role.EnvRunList).MapKeys()
		environments := make([]string, len(keys)+1)
		// Add _default environment
		environments[0] = "_default"
		for i, key := range keys {
			environments[i+1] = key.String()
		}

		cl[index] = &response.RoleListItem{
			Name:         role.Name,
			Description:  role.Description,
			Environments: environments,
		}
		index++
	}

	return cl
}

func findRoleFromRoleList(name string, result *RoleListResult) *chef.Role {
	for _, rItem := range result.Rows {
		if rItem.Name == name {
			return rItem
		}
	}
	return nil
}

// GetExpandRunlistFromRole expands the run-list based on role's run-list
func GetExpandRunlistFromRole(runlist []string, result *RoleListResult) ([]*response.RunList, error) {
	runList := make([]*response.RunList, len(runlist))
	for i, item := range runlist {
		newItem, err := chef.NewRunListItem(item)
		if err != nil {
			return nil, err
		}
		newRunList := response.RunList{
			Type:    newItem.Type,
			Name:    newItem.Name,
			Version: newItem.Version,
		}

		if newItem.IsRole() {
			currentRole := findRoleFromRoleList(newItem.Name, result)
			if currentRole != nil {
				newRunList.Children, _ = GetExpandRunlistFromRole(currentRole.RunList, result)
			}
		}
		runList[i] = &newRunList
	}
	return runList, nil
}

// fromAPIToRoleResponse a response.Role from a chef Role
func fromAPIToRoleResponse(role *chef.Role) (*response.Role, error) {
	defaultAttributes, err := json.Marshal(role.DefaultAttributes)
	if err != nil {
		return nil, status.Error(codes.Internal, err.Error())
	}

	overrideAttributes, err := json.Marshal(role.OverrideAttributes)
	if err != nil {
		return nil, status.Error(codes.Internal, err.Error())
	}

	return &response.Role{
		Name:               role.Name,
		ChefType:           role.ChefType,
		Description:        role.Description,
		JsonClass:          role.JsonClass,
		RunList:            role.RunList,
		DefaultAttributes:  string(defaultAttributes),
		OverrideAttributes: string(overrideAttributes),
	}, nil
}
