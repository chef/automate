package server

import (
	"context"
	"encoding/json"
	"reflect"

	chef "github.com/go-chef/chef"
	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/status"

	"github.com/chef/automate/api/interservice/infra_proxy/request"
	"github.com/chef/automate/api/interservice/infra_proxy/response"
	"github.com/chef/automate/components/infra-proxy-service/validation"
)

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

	return &response.Role{
		Name: req.Name,
	}, nil
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

	result, err := client.SearchObjectsWithDefaults("role", req.SearchQuery)
	if err != nil {
		return nil, err
	}

	return &response.Roles{
		Roles: fromAPIToListRoles(result),
		Page:  int32(result.Start),
		Total: int32(result.Total),
	}, nil
}

// GetRole get role appended with expanded runlist
// In order to get expanded runlist it required to have all roles if any
// RunList contains the another Role's RunList.
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

	result, err := c.SearchObjectsWithDefaults("role", &request.SearchQuery{})
	if err != nil {
		return nil, ParseAPIError(err)
	}

	role, err := c.client.Roles.Get(req.Name)
	if err != nil {
		return nil, ParseAPIError(err)
	}

	defaultAttributes, err := json.Marshal(role.DefaultAttributes)
	if err != nil {
		return nil, status.Error(codes.Internal, err.Error())
	}

	overrideAttributes, err := json.Marshal(role.OverrideAttributes)
	if err != nil {
		return nil, status.Error(codes.Internal, err.Error())
	}

	expandedRunList, err := toResponseExpandedRunList(role)
	if err != nil {
		return nil, status.Error(codes.Internal, err.Error())
	}

	return &response.Role{
		Name:               role.Name,
		ChefType:           role.ChefType,
		Description:        role.Description,
		DefaultAttributes:  string(defaultAttributes),
		OverrideAttributes: string(overrideAttributes),
		JsonClass:          role.JsonClass,
		RunList:            role.RunList,
		ExpandedRunList:    expandedRunList,
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

	_, err = c.client.Roles.Put(
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

	return &response.Role{
		Name: req.Name,
	}, nil
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

func toResponseExpandedRunList(role *chef.Role) ([]*response.ExpandedRunList, error) {
	envResExpandedRunList := make([]*response.ExpandedRunList, len(role.EnvRunList)+1)
	runList, err := getExpandRunlistFromRole(role.RunList)
	if err != nil {
		return nil, err
	}

	envResExpandedRunList[0] = &response.ExpandedRunList{
		Id:      "_default",
		RunList: runList,
	}
	index := 0
	for key, value := range role.EnvRunList {
		eRunList, err := GetExpandRunlistFromRole(value, result)
		if err != nil {
			return nil, err
		}

		envResExpandedRunList[index+1] = &response.ExpandedRunList{
			Id:      key,
			RunList: eRunList,
		}
		index++
	}

	return envResExpandedRunList, nil
}

// GetExpandRunlistFromRole expands the run-list based on role's run-list
func GetExpandRunlistFromRole(runlist []string) ([]*response.RunList, error) {
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
			currentRole := 
			if currentRole != nil {
				newRunList.Children, _ = GetExpandRunlistFromRole(currentRole.RunList, result)
			}
		}
		runList[i] = &newRunList
	}
	return runList, nil
}
