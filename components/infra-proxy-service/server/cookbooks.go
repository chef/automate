package server

import (
	"bytes"
	"context"
	"sort"

	chef "github.com/go-chef/chef"
	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/status"

	"github.com/chef/automate/api/interservice/infra_proxy/request"
	"github.com/chef/automate/api/interservice/infra_proxy/response"
)

// GetCookbooks get cookbooks list
func (s *Server) GetCookbooks(ctx context.Context, req *request.Cookbooks) (*response.Cookbooks, error) {

	c, err := s.createClient(ctx, req.OrgId, req.ServerId)
	if err != nil {
		return nil, err
	}

	cookbookList, err := c.client.Cookbooks.List()
	if err != nil {
		return nil, ParseAPIError(err)
	}

	return &response.Cookbooks{
		Cookbooks: fromAPIToListCookbooks(cookbookList),
	}, nil
}

// GetCookbookVersions get cookbook with all available versions
func (s *Server) GetCookbookVersions(ctx context.Context, req *request.CookbookVersions) (*response.CookbookVersions, error) {

	c, err := s.createClient(ctx, req.OrgId, req.ServerId)
	if err != nil {
		return nil, err
	}

	res, err := c.client.Cookbooks.GetAvailableVersions(req.Name, "")
	if err != nil {
		return nil, ParseAPIError(err)
	}

	cookbook, success := res[req.Name]
	if !success {
		return nil, status.Errorf(codes.Internal, "unable to fetch cookbook versions for %s", req.Name)
	}
	return &response.CookbookVersions{
		Name:     req.Name,
		Versions: fromAPICookbookVersionToVersions(cookbook.Versions),
	}, nil
}

// GetCookbook get cookbook detail
func (s *Server) GetCookbook(ctx context.Context, req *request.Cookbook) (*response.Cookbook, error) {

	c, err := s.createClient(ctx, req.OrgId, req.ServerId)
	if err != nil {
		return nil, err
	}

	if req.Name == "" {
		return nil, status.Error(codes.InvalidArgument, "must supply cookbook name")
	}

	version := req.Version
	if version == "" {
		version = "_latest"
	}

	cookbook, err := c.client.Cookbooks.GetVersion(req.Name, version)
	if err != nil {
		return nil, ParseAPIError(err)
	}

	return &response.Cookbook{
		CookbookName: cookbook.CookbookName,
		Name:         cookbook.Name,
		Version:      cookbook.Version,
		ChefType:     cookbook.ChefType,
		Frozen:       cookbook.Frozen,
		JsonClass:    cookbook.JsonClass,
		Files:        parseCookbookItems(cookbook.Files),
		Templates:    parseCookbookItems(cookbook.Templates),
		Attributes:   parseCookbookItems(cookbook.Attributes),
		Recipes:      parseCookbookItems(cookbook.Recipes),
		Definitions:  parseCookbookItems(cookbook.Definitions),
		Libraries:    parseCookbookItems(cookbook.Libraries),
		Providers:    parseCookbookItems(cookbook.Providers),
		Resources:    parseCookbookItems(cookbook.Resources),
		RootFiles:    parseCookbookItems(cookbook.RootFiles),
		Metadata:     parseCookbookMetadata(cookbook.Metadata),
		Access:       parseCookbookAccess(cookbook.Access),
	}, nil
}

// GetCookbookFileContent get the data file content of the cookbook
func (s *Server) GetCookbookFileContent(ctx context.Context, req *request.CookbookFileContent) (*response.CookbookFileContent, error) {
	var writer bytes.Buffer
	c, err := s.createClient(ctx, req.OrgId, req.ServerId)
	if err != nil {
		return nil, err
	}

	clientReq, err := c.client.NewRequest("GET", req.Url, nil)
	if err != nil {
		return nil, status.Errorf(codes.Internal, "client request error: %s", err.Error())
	}
	clientReq.Header.Set("Accept", "text/plain")
	res, err := c.client.Do(clientReq, &writer)
	if err != nil {
		return nil, status.Errorf(codes.Internal, "unable to fetch: %s", err.Error())
	}
	defer res.Body.Close() // nolint: errcheck

	return &response.CookbookFileContent{
		Content: writer.String(),
	}, nil
}

func parseCookbookItems(items []chef.CookbookItem) []*response.CookbookItem {
	cl := make([]*response.CookbookItem, len(items))
	for i, c := range items {
		cl[i] = &response.CookbookItem{
			Url:         c.Url,
			Path:        c.Path,
			Specificity: c.Specificity,
			Name:        c.Name,
			Checksum:    c.Checksum,
		}
	}
	return cl
}

func parseCookbookMetadata(md chef.CookbookMeta) *response.CookbookMeta {
	return &response.CookbookMeta{
		Name:            md.Name,
		Version:         md.Version,
		Description:     md.Description,
		LongDescription: md.LongDescription,
		Maintainer:      md.Maintainer,
		MaintainerEmail: md.MaintainerEmail,
		License:         md.License,
	}
}

func parseCookbookAccess(cc chef.CookbookAccess) *response.CookbookAccess {
	return &response.CookbookAccess{
		Read:   cc.Read,
		Create: cc.Create,
		Grant:  cc.Grant,
		Update: cc.Grant,
		Delete: cc.Delete,
	}
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

func fromAPICookbookVersionToVersions(cl []chef.CookbookVersion) []string {
	vs := make([]string, len(cl))
	for i, item := range cl {
		vs[i] = item.Version
	}

	return vs
}
