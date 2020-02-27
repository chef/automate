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

// GetEnvironments get environments list
func (s *Server) GetEnvironments(ctx context.Context, req *request.Environments) (*response.Environments, error) {

	client, err := s.createClient(ctx, req.OrgId)
	if err != nil {
		return nil, status.Errorf(codes.InvalidArgument, "invalid org id: %s", err.Error())
	}

	environments, err := client.Environments.List()
	if err != nil {
		return nil, status.Error(codes.InvalidArgument, err.Error())
	}

	return &response.Environments{
		Environments: fromAPIToListEnvironments(*environments),
	}, nil
}

// GetEnvironment get data bag
func (s *Server) GetEnvironment(ctx context.Context, req *request.Environment) (*response.Environment, error) {
	client, err := s.createClient(ctx, req.OrgId)
	if err != nil {
		return nil, status.Errorf(codes.InvalidArgument, "invalid org id: %s", err.Error())
	}

	en, err := client.Environments.Get(req.Name)
	if err != nil {
		return nil, status.Error(codes.InvalidArgument, err.Error())
	}

	return &response.Environment{
		Name:             en.Name,
		ChefType:         en.ChefType,
		Description:      en.Description,
		CookbookVersions: fromAPIToListEnvCookbookVersions(en.CookbookVersions),
		JsonClass:        en.JsonClass,
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

func fromAPIToListEnvCookbookVersions(cookbooks map[string]string) []string {
	cl := make([]string, len(cookbooks))

	for i, c := range cl {
		cl[i] = c
	}
	return cl
}
