package infra_proxy

import (
	"context"

	gwreq "github.com/chef/automate/api/external/infra_proxy/request"
	gwres "github.com/chef/automate/api/external/infra_proxy/response"
	infra_req "github.com/chef/automate/api/interservice/infra_proxy/request"
	infra_res "github.com/chef/automate/api/interservice/infra_proxy/response"
)

// GetNodes gets the nodes
func (a *InfraProxyServer) GetNodes(ctx context.Context, r *gwreq.Nodes) (*gwres.Nodes, error) {
	req := &infra_req.Nodes{
		OrgId:    r.OrgId,
		ServerId: r.ServerId,
		SearchQuery: &infra_req.SearchQuery{
			Q:       r.GetSearchQuery().GetQ(),
			Page:    r.GetSearchQuery().GetPage(),
			PerPage: r.GetSearchQuery().GetPerPage(),
		},
	}
	res, err := a.client.GetNodes(ctx, req)
	if err != nil {
		return nil, err
	}

	return &gwres.Nodes{
		Nodes: parseNodeAttributeFromRes(res.Nodes),
		Page:  res.GetPage(),
		Total: res.GetTotal(),
	}, nil
}

// DeleteNode deletes the node by name
func (a *InfraProxyServer) DeleteNode(ctx context.Context, r *gwreq.Node) (*gwres.DeleteNode, error) {
	req := &infra_req.Node{
		OrgId:    r.OrgId,
		ServerId: r.ServerId,
		Name:     r.Name,
	}
	res, err := a.client.DeleteNode(ctx, req)
	if err != nil {
		return nil, err
	}

	return &gwres.DeleteNode{
		Name: res.GetName(),
	}, nil
}

// GetNode gets the node
func (a *InfraProxyServer) GetNode(ctx context.Context, r *gwreq.Node) (*gwres.Node, error) {
	req := &infra_req.Node{
		OrgId:    r.OrgId,
		ServerId: r.ServerId,
		Name:     r.Name,
	}
	res, err := a.client.GetNode(ctx, req)
	if err != nil {
		return nil, err
	}

	return &gwres.Node{
		NodeId:              res.GetNodeId(),
		Name:                res.GetName(),
		Environment:         res.GetEnvironment(),
		RunList:             res.GetRunList(),
		Tags:                res.GetTags(),
		AutomaticAttributes: res.GetAutomaticAttributes(),
		DefaultAttributes:   res.GetDefaultAttributes(),
		NormalAttributes:    res.GetNormalAttributes(),
		OverrideAttributes:  res.GetOverrideAttributes(),
		PolicyName:          res.GetPolicyName(),
		PolicyGroup:         res.GetPolicyGroup(),
	}, nil
}

// UpdateNode updates the node attributes
func (a *InfraProxyServer) UpdateNode(ctx context.Context, r *gwreq.NodeDetails) (*gwres.Node, error) {
	req := &infra_req.NodeDetails{
		OrgId:               r.OrgId,
		ServerId:            r.ServerId,
		Name:                r.Name,
		Environment:         r.Environment,
		RunList:             r.RunList,
		AutomaticAttributes: r.AutomaticAttributes,
		DefaultAttributes:   r.DefaultAttributes,
		NormalAttributes:    r.NormalAttributes,
		OverrideAttributes:  r.OverrideAttributes,
		PolicyName:          r.PolicyName,
		PolicyGroup:         r.PolicyGroup,
	}
	res, err := a.client.UpdateNode(ctx, req)
	if err != nil {
		return nil, err
	}

	return &gwres.Node{
		NodeId:              res.GetNodeId(),
		Name:                res.GetName(),
		Environment:         res.GetEnvironment(),
		PolicyName:          res.GetPolicyName(),
		PolicyGroup:         res.GetPolicyGroup(),
		RunList:             res.GetRunList(),
		Tags:                res.GetTags(),
		AutomaticAttributes: res.GetAutomaticAttributes(),
		DefaultAttributes:   res.GetDefaultAttributes(),
		NormalAttributes:    res.GetNormalAttributes(),
		OverrideAttributes:  res.GetOverrideAttributes(),
	}, nil
}

// UpdateNodeTags updates the node tags
func (a *InfraProxyServer) UpdateNodeTags(ctx context.Context, r *gwreq.UpdateNodeTags) (*gwres.UpdateNodeTags, error) {
	req := &infra_req.UpdateNodeTags{
		OrgId:    r.OrgId,
		ServerId: r.ServerId,
		Name:     r.Name,
		Action:   r.Action,
		Tags:     r.Tags,
	}
	res, err := a.client.UpdateNodeTags(ctx, req)
	if err != nil {
		return nil, err
	}

	return &gwres.UpdateNodeTags{
		Tags: res.GetTags(),
	}, nil
}

// UpdateNodeEnvironment updates the node environment
func (a *InfraProxyServer) UpdateNodeEnvironment(ctx context.Context, r *gwreq.UpdateNodeEnvironment) (*gwres.UpdateNodeEnvironment, error) {
	req := &infra_req.UpdateNodeEnvironment{
		OrgId:       r.OrgId,
		ServerId:    r.ServerId,
		Name:        r.Name,
		Environment: r.Environment,
	}
	res, err := a.client.UpdateNodeEnvironment(ctx, req)
	if err != nil {
		return nil, err
	}

	return &gwres.UpdateNodeEnvironment{
		Name:        res.GetName(),
		Environment: res.GetEnvironment(),
	}, nil
}

// UpdateNodeAttributes updates the node attributes
func (a *InfraProxyServer) UpdateNodeAttributes(ctx context.Context, r *gwreq.UpdateNodeAttributes) (*gwres.UpdateNodeAttributes, error) {
	req := &infra_req.UpdateNodeAttributes{
		OrgId:      r.OrgId,
		ServerId:   r.ServerId,
		Name:       r.Name,
		Attributes: r.Attributes,
	}
	res, err := a.client.UpdateNodeAttributes(ctx, req)
	if err != nil {
		return nil, err
	}

	return &gwres.UpdateNodeAttributes{
		Name:       res.GetName(),
		Attributes: res.GetAttributes(),
	}, nil
}

func parseNodeAttributeFromRes(nodes []*infra_res.NodeAttribute) []*gwres.NodeAttribute {
	nl := make([]*gwres.NodeAttribute, len(nodes))

	for i, node := range nodes {
		nl[i] = &gwres.NodeAttribute{
			Id:          node.Id,
			Name:        node.Name,
			Fqdn:        node.Fqdn,
			IpAddress:   node.IpAddress,
			CheckIn:     node.CheckIn,
			Environment: node.Environment,
			Platform:    node.Platform,
			PolicyGroup: node.PolicyGroup,
			Uptime:      node.Uptime,
		}
	}

	return nl
}

func (a *InfraProxyServer) GetNodeExpandedRunList(ctx context.Context, r *gwreq.NodeExpandedRunList) (*gwres.NodeExpandedRunList, error) {
	req := &infra_req.NodeExpandedRunList{
		OrgId:       r.OrgId,
		ServerId:    r.ServerId,
		Name:        r.Name,
		Environment: r.Environment,
	}

	res, err := a.client.GetNodeExpandedRunList(ctx, req)
	if err != nil {
		return nil, err
	}

	return &gwres.NodeExpandedRunList{
		Id:      res.GetId(),
		RunList: fromUpsteamRunList(res.GetRunList()),
	}, nil
}
