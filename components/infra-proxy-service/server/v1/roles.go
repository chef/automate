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

// GetRoles get roles list
func (s *Server) GetRoles(ctx context.Context, req *request.Roles) (*response.Roles, error) {

	client, err := s.createClient(ctx, req.OrgId)
	if err != nil {
		return nil, status.Errorf(codes.InvalidArgument, "invalid org id: %s", err.Error())
	}

	roles, err := client.Roles.List()
	if err != nil {
		return nil, status.Error(codes.InvalidArgument, err.Error())
	}

	return &response.Roles{
		Roles: fromAPIToListRoles(*roles),
	}, nil
}

// GetRole get role
func (s *Server) GetRole(ctx context.Context, req *request.Role) (*response.Role, error) {
	client, err := s.createClient(ctx, req.OrgId)
	if err != nil {
		return nil, status.Errorf(codes.InvalidArgument, "invalid org id: %s", err.Error())
	}

	role, err := client.Roles.Get(req.Name)
	if err != nil {
		return nil, status.Error(codes.InvalidArgument, err.Error())
	}

	return &response.Role{
		Name:        role.Name,
		ChefType:    role.ChefType,
		Description: role.Description,
		RunList:     role.RunList,
		JsonClass:   role.JsonClass,
	}, nil

}

// fromAPIToListRoles a response.Roles from a struct of RoleList
func fromAPIToListRoles(al chef.RoleListResult) []*response.RoleListItem {
	cl := make([]*response.RoleListItem, len(al))

	index := 0
	for c := range al {
		cl[index] = &response.RoleListItem{
			Name: c,
		}
		index++
	}

	sort.Slice(cl, func(i, j int) bool {
		return cl[i].Name < cl[j].Name
	})

	return cl
}
