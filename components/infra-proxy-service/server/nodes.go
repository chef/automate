package server

import (
	"context"
	"encoding/json"
	"fmt"
	"log"

	chef "github.com/go-chef/chef"
	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/status"

	"github.com/chef/automate/api/interservice/infra_proxy/request"
	"github.com/chef/automate/api/interservice/infra_proxy/response"
	"github.com/chef/automate/components/infra-proxy-service/validation"

	log1 "github.com/sirupsen/logrus"
)

// GetNodes fetches the nodes from chef infra server
func (s *Server) GetNodes(ctx context.Context, req *request.Nodes) (*response.Nodes, error) {
	c, err := s.createClient(ctx, req.OrgId, req.ServerId)
	if err != nil {
		return nil, err
	}

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

	res, err := c.SearchObjectsWithDefaults("node", req.SearchQuery, params)
	if err != nil {
		return nil, err
	}

	return &response.Nodes{
		Nodes: fromSearchAPIToNodes(res),
		Page:  int32(res.Start),
		Total: int32(res.Total),
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
		return nil, ParseAPIError(err)
	}

	node, err := responseNodeObject(&res)
	if err != nil {
		return nil, err
	}

	return node, nil
}

// CreateNode creates the node
func (s *Server) CreateNode(ctx context.Context, req *request.NodeDetails) (*response.Node, error) {
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

	err = c.client.Clients.Delete(req.Name)
	if err != nil {
		return nil, fmt.Errorf("Node is deleted but client deletion failed: %s", err)
	}

	return &response.DeleteNode{
		Name: req.Name,
	}, nil
}

// UpdateNode updates the node
func (s *Server) UpdateNode(ctx context.Context, req *request.NodeDetails) (*response.Node, error) {
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
		tags = SubtractSlice(tags, req.Tags)
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

// UpdateNodeEnvironment updates the node environment
func (s *Server) UpdateNodeEnvironment(ctx context.Context, req *request.UpdateNodeEnvironment) (*response.UpdateNodeEnvironment, error) {
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

	chefNode, err := c.client.Nodes.Get(req.Name)
	if err != nil {
		return nil, ParseAPIError(err)
	}

	chefNode.Environment = req.Environment
	res, err := c.client.Nodes.Put(chefNode)
	if err != nil {
		return nil, ParseAPIError(err)
	}

	return &response.UpdateNodeEnvironment{
		Name:        res.Name,
		Environment: res.Environment,
	}, nil

}

// UpdateNodeAttributes updates the node attributes
func (s *Server) UpdateNodeAttributes(ctx context.Context, req *request.UpdateNodeAttributes) (*response.UpdateNodeAttributes, error) {
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

	attributes, err := StructToJSON(req.Attributes)
	if err != nil {
		return nil, err
	}

	chefNode, err := c.client.Nodes.Get(req.Name)
	if err != nil {
		return nil, ParseAPIError(err)
	}

	chefNode.NormalAttributes = attributes.(map[string]interface{})
	res, err := c.client.Nodes.Put(chefNode)
	if err != nil {
		return nil, ParseAPIError(err)
	}

	resAttributes, err := json.Marshal(res.NormalAttributes)
	if err != nil {
		return nil, status.Error(codes.Internal, err.Error())
	}

	return &response.UpdateNodeAttributes{
		Name:       res.Name,
		Attributes: string(resAttributes),
	}, nil
}

// GetNodeExpandedRunList fetches the expanded runlist of a node
func (s *Server) GetNodeExpandedRunList(ctx context.Context, req *request.NodeExpandedRunList) (*response.NodeExpandedRunList, error) {
	err := validation.New(validation.Options{
		Target:  "role",
		Request: *req,
		Rules: validation.Rules{
			"OrgId":       []string{"required"},
			"ServerId":    []string{"required"},
			"Name":        []string{"required"},
			"Environment": []string{"required"},
		},
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
		return nil, ParseAPIError(err)
	}

	runList := res.RunList

	if res.PolicyGroup != "" {
		req.Environment = "_default"
	}

	log.Printf("Environment: %+v", res.Environment)
	cookbooks, err := c.client.Environments.ListCookbooks(req.Environment, "1")
	if err != nil {
		log1.Info("Failed while listing cookbooks for default")
		log1.Error(err)
		return nil, ParseAPIError(err)
	}

	log.Printf("defaultCookbooks: %+v", cookbooks)

	var runlistCache = RunListCache{}

	var expandedRunList []*response.RunList
	expandedRunList, err = ToResponseExpandedRunList(c, runList, cookbooks, runlistCache)
	if err != nil {
		return nil, ParseAPIError(err)
	}

	return &response.NodeExpandedRunList{
		Id:      req.Environment,
		RunList: expandedRunList,
	}, nil
}

// fetchAffectedNodes gets the nodes used by chef object
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
func fromSearchAPIToNodes(sr *chef.SearchResult) []*response.NodeAttribute {
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

func nodeAttributeFromParams(req *request.NodeDetails) (*chef.Node, error) {
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
