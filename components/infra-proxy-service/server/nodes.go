package server

import (
	"context"
	"fmt"
	"strconv"

	chef "github.com/go-chef/chef"

	"github.com/chef/automate/api/interservice/infra_proxy/request"
	"github.com/chef/automate/api/interservice/infra_proxy/response"
	"github.com/chef/automate/components/infra-proxy-service/validation"
)

// GetAffectedNodes get the nodes using chef object
func (s *Server) GetAffectedNodes(ctx context.Context, req *request.AffectedNodes) (*response.AffectedNodes, error) {
	c, err := s.createClient(ctx, req.OrgId, req.ServerId)
	if err != nil {
		return nil, err
	}

	res, err := c.fetchAffectedNodes(ctx, req.ChefType, req.Name, req.Version)
	if err != nil {
		return nil, err
	}

	return &response.AffectedNodes{
		Nodes: fromSearchAPIToAffectedNodes(*res),
	}, nil
}

// DeleteNode deletes the node by name
func (s *Server) DeleteNode(ctx context.Context, req *request.DeleteNode) (*response.DeleteNode, error) {
	err := validation.New(validation.Options{
		Target:  "node",
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

	err = c.client.Nodes.Delete(req.Name)
	if err != nil {
		return nil, err
	}

	return &response.DeleteNode{
		Name: req.Name,
	}, nil
}

// UpdateNode update the node attributes
func (s *Server) UpdateNode(ctx context.Context, req *request.UpdateNode) (*response.UpdateNode, error) {
	c, err := s.createClient(ctx, req.OrgId, req.ServerId)
	if err != nil {
		return nil, err
	}

	automatic, err := StructToJSON(req.AutomaticAttributes)
	if err != nil {
		return nil, err
	}

	normal, err := StructToJSON(req.NormalAttributes)
	if err != nil {
		return nil, err
	}

	defaults, err := StructToJSON(req.DefaultAttributes)
	if err != nil {
		return nil, err
	}

	override, err := StructToJSON(req.OverrideAttributes)
	if err != nil {
		return nil, err
	}

	_, err = c.client.Nodes.Put(chef.Node{
		Name:                req.Name,
		Environment:         req.Environment,
		RunList:             req.RunList,
		AutomaticAttributes: automatic.(map[string]interface{}),
		NormalAttributes:    normal.(map[string]interface{}),
		DefaultAttributes:   defaults.(map[string]interface{}),
		OverrideAttributes:  override.(map[string]interface{}),
		PolicyName:          req.PolicyName,
		PolicyGroup:         req.PolicyGroup,
	})
	if err != nil {
		return nil, err
	}

	return &response.UpdateNode{
		Name: req.Name,
	}, nil
}

// fetchAffectedNodes get the nodes used by chef object
// URL is being constructed based on the chefType, name, and version
// chefType: should be one of the cookbooks, roles and chef_environment value
// For cookbooks: it would be 'cookbooks_COOKBOOK_NAME_version:COOKBOOK_VERSION'
// For other chef objects
// roles: it would be 'roles:ROLE_NAME'
// environments: it would be 'chef_environment:ENVIRONMENT_NAME'
// policyfiles: it would be 'chef_environment:POLICY_FILES_GROUP_NAME'
func (c *ChefClient) fetchAffectedNodes(ctx context.Context, chefType, name, version string) (*chef.SearchResult, error) {
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
		url = fmt.Sprintf("%s_%s_version:%s", chefType, name, version)
	} else {
		url = fmt.Sprintf("%s:%s", chefType, name)
	}

	res, err := c.client.Search.PartialExec("node", url, query)
	if err != nil {
		return nil, ParseAPIError(err)
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
			Id:          safeStringFromMap(m, "chef_guid"),
			Name:        safeStringFromMap(m, "name"),
			CheckIn:     safeStringFromMapFloat(m, "ohai_time"),
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
