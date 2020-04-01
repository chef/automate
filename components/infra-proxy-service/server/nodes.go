package server

import (
	"context"
	"fmt"
	"strconv"

	chef "github.com/chef/go-chef"
	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/status"

	"github.com/chef/automate/api/interservice/infra_proxy/request"
	"github.com/chef/automate/api/interservice/infra_proxy/response"
)

// NodeAttribute attributes of the node
type NodeAttribute struct {
	Name            string
	ChefGUID        string
	CheckIn         string
	ChefEnvironment string
	Platform        string
	PolicyGroup     string
	Uptime          string
}

// GetAffectedNodes get the nodes using resource
func (s *Server) GetAffectedNodes(ctx context.Context, req *request.AffectedNodes) (*response.AffectedNodes, error) {
	c, err := s.createClient(ctx, req.OrgId)
	if err != nil {
		return nil, status.Errorf(codes.InvalidArgument, "invalid org ID: %s", err.Error())
	}

	res, err := c.FetchAffectedNodes(ctx, req.Resource, req.Name, req.Version)

	return &response.AffectedNodes{
		Nodes: fromSearchAPIToAffectedNodes(*res),
	}, nil
}

// FetchAffectedNodes get the nodes used by resource
func (c *ChefClient) FetchAffectedNodes(ctx context.Context, resource, name, version string) (*chef.SearchResult, error) {
	query := map[string]interface{}{
		"name":             []string{"name"},
		"platform":         []string{"platform"},
		"chef_environment": []string{"chef_environment"},
		"policy_group":     []string{"policy_group"},
		"chef_guid":        []string{"chef_guid"},
		"uptime":           []string{"uptime"},
		"ohai_time":        []string{"ohai_time"},
	}
	var url string
	if version != "" {
		url = fmt.Sprintf("%s_%s_version:%s", resource, name, version)
	} else {
		url = fmt.Sprintf("%s:%s", resource, name)
	}

	res, err := c.client.Search.PartialExec("node", url, query)
	if err != nil {
		return nil, status.Errorf(codes.Internal, err.Error())
	}

	return &res, nil
}

// This return the response node attributes array
// parse by the getting the attributes from partial search query.
func fromSearchAPIToAffectedNodes(sr chef.SearchResult) []*response.NodeAttribute {
	results := make([]*response.NodeAttribute, len(sr.Rows))
	index := 0
	for _, element := range sr.Rows {
		m := element.(map[string]interface{})["data"].(map[string]interface{})
		results[index] = &response.NodeAttribute{
			Name:        safeStringFromMap(m, "name"),
			CheckIn:     safeStringFromMapFloat(m, "ohai_time"),
			ChefGuid:    safeStringFromMap(m, "chef_guid"),
			Environment: safeStringFromMap(m, "chef_environment"),
			Platform:    safeStringFromMap(m, "platform"),
			PolicyGroup: safeStringFromMap(m, "policy_group"),
			Uptime:      safeStringFromMap(m, "uptime"),
		}
		index++
	}

	return results
}

// This returns the value referenced by `key` in `values`. If value is nil,
// it returns an empty string; otherwise it returns the original string.
func safeStringFromMap(values map[string]interface{}, key string) string {
	if values[key] == nil {
		return ""
	}
	return values[key].(string)
}

// This returns the value referenced by `key` in `values`. If value is nil,
// it returns an empty string; otherwise it returns the base 64 float string.
func safeStringFromMapFloat(values map[string]interface{}, key string) string {
	if values[key] == nil {
		return ""
	}
	return strconv.FormatFloat(values[key].(float64), 'E', -1, 64)
}
