package server

import (
	"context"
	"encoding/json"
	"fmt"

	chef "github.com/go-chef/chef"
	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/status"

	"github.com/chef/automate/api/interservice/infra_proxy/request"
	"github.com/chef/automate/api/interservice/infra_proxy/response"
	"github.com/chef/automate/components/infra-proxy-service/validation"
)

// GetNodes fetches the nodes from chef infra server
func (s *Server) GetNodes(ctx context.Context, req *request.Nodes) (*response.Nodes, error) {
	c, err := s.createClient(ctx, req.OrgId, req.ServerId)
	if err != nil {
		return nil, err
	}

	res, err := c.fetchAffectedNodes(ctx, "*:*")
	if err != nil {
		return nil, err
	}

	return &response.Nodes{
		Nodes: fromSearchAPIToAffectedNodes(*res),
	}, nil
}

// GetAffectedNodes get the nodes using chef object
func (s *Server) GetAffectedNodes(ctx context.Context, req *request.AffectedNodes) (*response.AffectedNodes, error) {
	c, err := s.createClient(ctx, req.OrgId, req.ServerId)
	if err != nil {
		return nil, err
	}

	var statement string
	if req.Version != "" {
		statement = fmt.Sprintf("%s_%s_version:%s", req.ChefType, req.Name, req.Version)
	} else {
		statement = fmt.Sprintf("%s:%s", req.ChefType, req.Name)
	}

	res, err := c.fetchAffectedNodes(ctx, statement)
	if err != nil {
		return nil, err
	}

	return &response.AffectedNodes{
		Nodes: fromSearchAPIToAffectedNodes(*res),
	}, nil
}

// GetNode fetches the node from chef infra server
func (s *Server) GetNode(ctx context.Context, req *request.Node) (*response.Node, error) {
	err := validation.New(validation.Options{
		Target:          "node",
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

	res, err := c.client.Nodes.Get(req.Name)
	if err != nil {
		return nil, err
	}

	node, err := responseNodeObject(&res)
	if err != nil {
		return nil, err
	}

	return node, nil
}

// CreateNode creates the node
func (s *Server) CreateNode(ctx context.Context, req *request.NodeObject) (*response.Node, error) {
	c, err := s.createClient(ctx, req.OrgId, req.ServerId)
	if err != nil {
		return nil, err
	}

	reqNode, err := nodeAttributeFromParams(req)
	if err != nil {
		return nil, err
	}

	_, err = c.client.Nodes.Post(*reqNode)
	if err != nil {
		return nil, err
	}

	// Fetches the created node
	res, err := c.client.Nodes.Get(reqNode.Name)
	if err != nil {
		return nil, err
	}

	node, err := responseNodeObject(&res)
	if err != nil {
		return nil, err
	}

	return node, nil
}

// DeleteNode deletes the node by name
func (s *Server) DeleteNode(ctx context.Context, req *request.Node) (*response.DeleteNode, error) {
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

// UpdateNode updates the node
func (s *Server) UpdateNode(ctx context.Context, req *request.NodeObject) (*response.Node, error) {
	c, err := s.createClient(ctx, req.OrgId, req.ServerId)
	if err != nil {
		return nil, err
	}

	reqNode, err := nodeAttributeFromParams(req)
	if err != nil {
		return nil, err
	}

	res, err := c.client.Nodes.Put(*reqNode)
	if err != nil {
		return nil, err
	}

	node, err := responseNodeObject(&res)
	if err != nil {
		return nil, err
	}

	return node, nil
}

// UpdateNodeTags updates the tags
func (s *Server) UpdateNodeTags(ctx context.Context, req *request.UpdateNodeTags) (*response.UpdateNodeTags, error) {
	err := validation.New(validation.Options{
		Target:  "node",
		Request: *req,
		Rules: validation.Rules{
			"OrgId":    []string{"required"},
			"ServerId": []string{"required"},
			"Name":     []string{"required"},
			"Action":   []string{"required"},
		},
	}).Validate()

	if err != nil {
		return nil, err
	}

	c, err := s.createClient(ctx, req.OrgId, req.ServerId)
	if err != nil {
		return nil, err
	}

	chefNode, err := c.client.Nodes.Get(req.Name)
	if err != nil {
		return nil, ParseAPIError(err)
	}

	tags := SafeSliceFromMap(chefNode.NormalAttributes, "tags")
	switch req.Action {
	case "add":
		tags = Unique(append(tags, req.Tags...))
	case "delete":
		tags = SubstractSlice(tags, req.Tags)
	case "set":
		tags = req.Tags
	default:
		return nil, status.Errorf(codes.InvalidArgument, "Invalid node action: %s", req.Action)
	}

	chefNode.NormalAttributes["tags"] = tags
	res, err := c.client.Nodes.Put(chefNode)
	if err != nil {
		return nil, ParseAPIError(err)
	}

	return &response.UpdateNodeTags{
		Tags: SafeSliceFromMap(res.NormalAttributes, "tags"),
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
func (c *ChefClient) fetchAffectedNodes(ctx context.Context, statement string) (*chef.SearchResult, error) {
	params := map[string]interface{}{
		"name":             []string{"name"},
		"fqdn":             []string{"fqdn"},
		"ipaddress":        []string{"ipaddress"},
		"platform":         []string{"platform"},
		"chef_environment": []string{"chef_environment"},
		"policy_group":     []string{"policy_group"},
		"chef_guid":        []string{"chef_guid"},
		"uptime":           []string{"uptime"},
		"ohai_time":        []string{"ohai_time"},
	}

	res, err := c.client.Search.PartialExec("node", statement, params)
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
			Id:          SafeStringFromMap(m, "chef_guid"),
			Name:        SafeStringFromMap(m, "name"),
			Fqdn:        SafeStringFromMap(m, "fqdn"),
			IpAddress:   SafeStringFromMap(m, "ipaddress"),
			CheckIn:     SafeStringFromMapFloat(m, "ohai_time"),
			Environment: SafeStringFromMap(m, "chef_environment"),
			Platform:    SafeStringFromMap(m, "platform"),
			PolicyGroup: SafeStringFromMap(m, "policy_group"),
			Uptime:      SafeStringFromMap(m, "uptime"),
		}
		index++
	}

	return results
}

func nodeAttributeFromParams(req *request.NodeObject) (*chef.Node, error) {

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

	node := &chef.Node{
		Name:                req.Name,
		Environment:         req.Environment,
		RunList:             req.RunList,
		AutomaticAttributes: automatic.(map[string]interface{}),
		NormalAttributes:    normal.(map[string]interface{}),
		DefaultAttributes:   defaults.(map[string]interface{}),
		OverrideAttributes:  override.(map[string]interface{}),
		PolicyName:          req.PolicyName,
		PolicyGroup:         req.PolicyGroup,
	}

	return node, nil
}

func responseNodeObject(node *chef.Node) (*response.Node, error) {
	defaultAttributes, err := json.Marshal(node.DefaultAttributes)
	if err != nil {
		return nil, status.Error(codes.Internal, err.Error())
	}

	automaticAttributes, err := json.Marshal(node.AutomaticAttributes)
	if err != nil {
		return nil, status.Error(codes.Internal, err.Error())
	}

	normalAttributes, err := json.Marshal(node.NormalAttributes)
	if err != nil {
		return nil, status.Error(codes.Internal, err.Error())
	}

	overrideAttributes, err := json.Marshal(node.OverrideAttributes)
	if err != nil {
		return nil, status.Error(codes.Internal, err.Error())
	}

	return &response.Node{
		NodeId:              node.Name,
		Name:                node.Name,
		Environment:         node.Environment,
		PolicyName:          node.PolicyName,
		PolicyGroup:         node.PolicyGroup,
		RunList:             node.RunList,
		Tags:                SafeSliceFromMap(node.NormalAttributes, "tags"),
		DefaultAttributes:   string(defaultAttributes),
		AutomaticAttributes: string(automaticAttributes),
		NormalAttributes:    string(normalAttributes),
		OverrideAttributes:  string(overrideAttributes),
	}, nil
}
