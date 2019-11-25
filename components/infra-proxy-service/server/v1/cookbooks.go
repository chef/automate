package v1

import (
	"context"
	"sort"

	proxy "github.com/chef/automate/components/infra-proxy-service/proxy"
	"github.com/chef/automate/components/infra-proxy-service/service"
	uuid "github.com/chef/automate/lib/uuid4"
	"github.com/go-chef/chef"
	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/status"

	"github.com/chef/automate/api/interservice/infra_proxy/request"
	"github.com/chef/automate/api/interservice/infra_proxy/response"
)

// GetCookbooks get cookbooks list
func (s *Server) GetCookbooks(ctx context.Context, req *request.Cookbooks) (*response.Cookbooks, error) {

	client, err := s.createClient(ctx, req.OrgId)
	if err != nil {
		return nil, status.Errorf(codes.InvalidArgument, "invalid org id: %s", err.Error())
	}

	cookbookList, err := client.GetCookbooks()
	if err != nil {
		return nil, status.Errorf(codes.InvalidArgument, "invalid org id: %s", err.Error())
	}

	return &response.Cookbooks{
		Cookbooks: fromAPIToListCookbooks(cookbookList),
	}, nil
}

// GetCookbooksAvailableVersions get cookbooks list with all available versions
func (s *Server) GetCookbooksAvailableVersions(ctx context.Context, req *request.CookbooksAvailableVersions) (*response.CookbooksAvailableVersions, error) {

	client, err := s.createClient(ctx, req.OrgId)
	if err != nil {
		return nil, status.Errorf(codes.InvalidArgument, "invalid org id: %s", err.Error())
	}

	numVersions := req.NumVersions

	if numVersions == "" {
		numVersions = "all"
	}

	cookbookList, err := client.GetCookbooksAvailableVersions(numVersions)
	if err != nil {
		return nil, status.Errorf(codes.InvalidArgument, "invalid org id: %s", err.Error())
	}

	return &response.CookbooksAvailableVersions{
		Cookbooks: fromAPIToListAvailableCookbooks(cookbookList),
	}, nil
}

// GetCookbook get cookbooks list
func (s *Server) GetCookbook(ctx context.Context, req *request.Cookbook) (*response.Cookbook, error) {

	// client, err := s.createClient(ctx, req)
	// if err != nil {
	// 	return nil, status.Errorf(codes.InvalidArgument, "invalid org id: %s", err.Error())
	// }

	// cookbook, err := client.GetCookbook("learn_chef_apache2", "latest")
	// if err != nil {
	// 	return nil, status.Errorf(codes.InvalidArgument, "invalid org id: %s", err.Error())
	// }

	return nil, nil
}

// fromAPIToListCookbooks a response.Cookbooks from a struct of CookbookListResult
func fromAPIToListCookbooks(al chef.CookbookListResult) []*response.CookbookVersion {
	cl := make([]*response.CookbookVersion, len(al))

	index := 0

	for k, v := range al {
		cl[index] = &response.CookbookVersion{
			Name:    k,
			Version: v.Versions[0].Version,
		}
		index++
	}

	sort.Slice(cl, func(i, j int) bool {
		return cl[i].Name < cl[j].Name
	})

	return cl
}

// fromAPIToListAvailableCookbooks a response.Cookbooks from a struct of CookbookListResult
func fromAPIToListAvailableCookbooks(al chef.CookbookListResult) []*response.CookbookAllVersion {
	cl := make([]*response.CookbookAllVersion, len(al))

	index := 0
	for k, v := range al {
		versions := make([]string, len(v.Versions))

		for i, c := range v.Versions {
			versions[i] = c.Version
		}

		cl[index] = &response.CookbookAllVersion{
			Name:           k,
			CurrentVersion: v.Versions[0].Version,
			Versions:       versions,
		}
		index++
	}

	sort.Slice(cl, func(i, j int) bool {
		return cl[i].Name < cl[j].Name
	})

	return cl
}

func (s *Server) createClient(ctx context.Context, orgID string) (*proxy.ChefClient, error) {
	ctx, cancel := context.WithCancel(ctx)
	defer cancel()

	UUID, err := uuid.FromString(orgID)
	if err != nil {
		return nil, status.Errorf(codes.InvalidArgument, "invalid org id: %s", err.Error())
	}

	org, err := s.service.Storage.GetOrg(ctx, UUID)
	if err != nil {
		return nil, service.ParseStorageError(err, orgID, "org")
	}

	ServerID, err := uuid.FromString(org.ServerId)
	if err != nil {
		return nil, status.Errorf(codes.InvalidArgument, "invalid org id: %s", err.Error())
	}

	server, err := s.service.Storage.GetServer(ctx, ServerID)
	if err != nil {
		return nil, service.ParseStorageError(err, ServerID, "org")
	}

	baseURL := server.IpAddress + "/organizations/" + org.Name + "/"

	client, err := proxy.NewChefClient(&proxy.ChefConfig{
		Name:    org.AdminUser,
		Key:     org.AdminKey,
		SkipSSL: true,
		BaseURL: baseURL,
	})

	return client, nil
}
