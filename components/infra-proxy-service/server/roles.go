package server

import (
	"context"
	"encoding/json"
	"reflect"
	"sort"

	chef "github.com/chef/go-chef"
	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/status"

	"github.com/chef/automate/api/interservice/infra_proxy/request"
	"github.com/chef/automate/api/interservice/infra_proxy/response"
)

// RunList represents the recipes and roles specified for a node or as part of a role.
type RunList []string

// EnvRunList represents the recipes and roles with environment specified for a node or as part of a role.
type EnvRunList map[string]RunList

// Role represents the native Go version of the deserialized Role type
type Role struct {
	Name               string      `json:"name"`
	ChefType           string      `json:"chef_type"`
	Description        string      `json:"description"`
	RunList            RunList     `json:"run_list"`
	EnvRunList         EnvRunList  `json:"env_run_lists"`
	DefaultAttributes  interface{} `json:"default_attributes,omitempty"`
	OverrideAttributes interface{} `json:"override_attributes,omitempty"`
	JSONClass          string      `json:"json_class,omitempty"`
}

// RoleListResult role list result from Search API
type RoleListResult struct {
	Total int     `json:"total"`
	Start int     `json:"start"`
	Rows  []*Role `json:"rows"`
}

// GetRoles get roles list
func (s *Server) GetRoles(ctx context.Context, req *request.Roles) (*response.Roles, error) {

	client, err := s.createClient(ctx, req.OrgId)
	if err != nil {
		return nil, status.Errorf(codes.InvalidArgument, "invalid org ID: %s", err.Error())
	}

	result := RoleListResult{}
	// Fetch roles using search API
	newReq, err := client.client.NewRequest("GET", "search/role", nil)
	if err != nil {
		return nil, status.Error(codes.Internal, err.Error())
	}

	res, err := client.client.Do(newReq, &result)
	if err != nil {
		return nil, status.Error(codes.Internal, err.Error())
	}

	defer res.Body.Close()

	return &response.Roles{
		Roles: fromAPIToListRoles(result),
	}, nil
}

// GetRole get role appended with expanded runlist
// In order to get expanded runlist it required to have all roles if any
// RunList contains the another Role's RunList.
func (s *Server) GetRole(ctx context.Context, req *request.Role) (*response.Role, error) {
	c, err := s.createClient(ctx, req.OrgId)
	if err != nil {
		return nil, status.Errorf(codes.InvalidArgument, "invalid org ID: %s", err.Error())
	}

	result := RoleListResult{}
	newReq, err := c.client.NewRequest("GET", "search/role", nil)
	if err != nil {
		return nil, status.Error(codes.Internal, err.Error())
	}

	res1, err := c.client.Do(newReq, &result)
	if err != nil {
		return nil, status.Error(codes.Internal, err.Error())
	}

	defer res1.Body.Close()

	if result.Total == 0 {
		return nil, status.Error(codes.Internal, err.Error())
	}

	role := findRoleFromRoleList(req.Name, &result)

	defaultAttributes, err := json.Marshal(role.DefaultAttributes)
	if err != nil {
		return nil, status.Error(codes.Internal, err.Error())
	}

	overrideAttributes, err := json.Marshal(role.OverrideAttributes)
	if err != nil {
		return nil, status.Error(codes.Internal, err.Error())
	}

	expandedRunList, err := toResponseExpandedRunList(role, &result)
	if err != nil {
		return nil, status.Error(codes.Internal, err.Error())
	}

	return &response.Role{
		Name:               role.Name,
		ChefType:           role.ChefType,
		Description:        role.Description,
		DefaultAttributes:  string(defaultAttributes),
		OverrideAttributes: string(overrideAttributes),
		JsonClass:          role.JSONClass,
		RunList:            role.RunList,
		ExpandedRunList:    expandedRunList,
	}, nil

}

// fromAPIToListRoles a response.Roles from a struct of RoleList
func fromAPIToListRoles(result RoleListResult) []*response.RoleListItem {
	cl := make([]*response.RoleListItem, result.Total)

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

	sort.Slice(cl, func(i, j int) bool {
		return cl[i].Name < cl[j].Name
	})

	return cl
}

func findRoleFromRoleList(name string, result *RoleListResult) *Role {
	for _, rItem := range result.Rows {
		if rItem.Name == name {
			return rItem
		}
	}
	return nil
}

func toResponseExpandedRunList(role *Role, result *RoleListResult) ([]*response.ExpandedRunList, error) {
	envResExpandedRunList := make([]*response.ExpandedRunList, len(role.EnvRunList)+1)

	runList, err := expandRunlistFromRole(role.RunList, result)
	if err != nil {
		return nil, err
	}

	envResExpandedRunList[0] = &response.ExpandedRunList{
		Id:      "_default",
		RunList: runList,
	}
	index := 0
	for key, value := range role.EnvRunList {
		eRunList, err := expandRunlistFromRole(value, result)
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

func expandRunlistFromRole(runlist []string, result *RoleListResult) ([]*response.RunList, error) {
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
			newRunList.Children, _ = expandRunlistFromRole(currentRole.RunList, result)
		}

		runList[i] = &newRunList
	}
	return runList, nil
}
