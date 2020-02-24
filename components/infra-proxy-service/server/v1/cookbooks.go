package v1

import (
	"context"
	"fmt"
	"sort"

	chef "github.com/chef/go-chef"
	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/status"

	"github.com/chef/automate/api/interservice/infra_proxy/request"
	"github.com/chef/automate/api/interservice/infra_proxy/response"
)

// NodeAttribute attributes of the node
type NodeAttribute struct {
	Name            string `json:"name"`
	ChefGUID        string `json:"chef_guid"`
	CheckIn         string `json:"checkin"`
	ChefEnvironment string `json:"chef_environment"`
	Platform        string `json:"platform"`
	PolicyGroup     string `json:"policy_group"`
	Uptime          string `json:"uptime"`
}

// GetCookbooks get cookbooks list
func (s *Server) GetCookbooks(ctx context.Context, req *request.Cookbooks) (*response.Cookbooks, error) {

	client, err := s.createClient(ctx, req.OrgId)
	if err != nil {
		return nil, status.Errorf(codes.InvalidArgument, "invalid org id: %s", err.Error())
	}

	cookbookList, err := client.Cookbooks.List()
	if err != nil {
		return nil, status.Errorf(codes.InvalidArgument, err.Error())
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

	cookbookList, err := client.Cookbooks.ListAvailableVersions(numVersions)
	if err != nil {
		return nil, status.Errorf(codes.InvalidArgument, err.Error())
	}

	return &response.CookbooksAvailableVersions{
		Cookbooks: fromAPIToListAvailableCookbooks(cookbookList),
	}, nil
}

// GetCookbook get cookbook detail
func (s *Server) GetCookbook(ctx context.Context, req *request.Cookbook) (*response.Cookbook, error) {

	client, err := s.createClient(ctx, req.OrgId)
	if err != nil {
		return nil, status.Errorf(codes.InvalidArgument, "invalid org id: %s", err.Error())
	}

	if req.Name == "" {
		s.service.Logger.Debug("Cookbook Fetch: missing cookbook name")
		return nil, status.Error(codes.InvalidArgument, "must supply cookbook name")
	}

	version := req.Version

	if version == "" {
		version = "_latest"
	}

	cookbook, err := client.Cookbooks.GetVersion(req.Name, version)
	if err != nil {
		return nil, status.Errorf(codes.InvalidArgument, err.Error())
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

// GetCookbookAffectedNodes get the nodes using cookbook
func (s *Server) GetCookbookAffectedNodes(ctx context.Context, req *request.Cookbook) (*response.CookbookAffectedNodes, error) {

	query := map[string]interface{}{
		"name":             []string{"name"},
		"platform":         []string{"platform"},
		"chef_environment": []string{"chef_environment"},
		"policy_group":     []string{"policy_group"},
		"chef_guid":        []string{"chef_guid"},
		"uptime":           []string{"uptime"},
		"ohai_time":        []string{"ohai_time"},
	}

	client, err := s.createClient(ctx, req.OrgId)
	if err != nil {
		return nil, status.Errorf(codes.InvalidArgument, "invalid org id: %s", err.Error())
	}

	res, err := client.Search.PartialExec("node", fmt.Sprintf("cookbooks_%s_version:%s", req.Name, req.Version), query)

	return &response.CookbookAffectedNodes{
		Nodes: fromSearchAPIToCookbookNodes(res),
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

func fromSearchAPIToCookbookNodes(sr chef.SearchResult) []*response.NodeAttribute {
	results := make([]*response.NodeAttribute, len(sr.Rows))
	index := 0
	for _, element := range sr.Rows {
		node := getNodeAttributeFromRes(element)
		results[index] = &response.NodeAttribute{
			Name:        node.Name,
			CheckIn:     node.CheckIn,
			ChefGuid:    node.ChefGUID,
			Environment: node.ChefEnvironment,
			Platform:    node.Platform,
			PolicyGroup: node.PolicyGroup,
			Uptime:      node.Uptime,
		}
		index++
	}

	return results
}

func getNodeAttributeFromRes(data interface{}) NodeAttribute {
	m := data.(map[string]interface{})["data"].(map[string]interface{})
	node := NodeAttribute{}
	if name, ok := m["name"].(string); ok {
		node.Name = name
	}

	if checkin, ok := m["ohai_time"].(string); ok {
		node.CheckIn = checkin
	}

	if chefGUID, ok := m["chef_guid"].(string); ok {
		node.ChefGUID = chefGUID
	}

	if environment, ok := m["chef_environment"].(string); ok {
		node.ChefEnvironment = environment
	}

	if platform, ok := m["platform"].(string); ok {
		node.Platform = platform
	}

	if policyGroup, ok := m["policy_group"].(string); ok {
		node.PolicyGroup = policyGroup
	}

	if uptime, ok := m["uptime"].(string); ok {
		node.Uptime = uptime
	}

	return node
}
