package infra_proxy

import (
	"context"

	gwreq "github.com/chef/automate/api/external/infra_proxy/request"
	gwres "github.com/chef/automate/api/external/infra_proxy/response"
	infra_req "github.com/chef/automate/api/interservice/infra_proxy/request"
	infra_res "github.com/chef/automate/api/interservice/infra_proxy/response"
)

// GetNodes get the nodes
func (a *InfraProxyServer) GetNodes(ctx context.Context, r *gwreq.Nodes) (*gwres.Nodes, error) {
	req := &infra_req.Nodes{
		OrgId:    r.OrgId,
		ServerId: r.ServerId,
	}
	res, err := a.client.GetNodes(ctx, req)
	if err != nil {
		return nil, err
	}

	return &gwres.Nodes{
		Nodes: parseNodeAttributeFromRes(res.Nodes),
	}, nil
}

// GetAffectedNodes get the nodes using resource
func (a *InfraProxyServer) GetAffectedNodes(ctx context.Context, r *gwreq.AffectedNodes) (*gwres.AffectedNodes, error) {

	req := &infra_req.AffectedNodes{
		OrgId:    r.OrgId,
		ServerId: r.ServerId,
		ChefType: r.ChefType,
		Name:     r.Name,
		Version:  r.Version,
	}
	res, err := a.client.GetAffectedNodes(ctx, req)
	if err != nil {
		return nil, err
	}

	return &gwres.AffectedNodes{
		Nodes: parseNodeAttributeFromRes(res.Nodes),
	}, nil
}

// DeleteNode deletes the node by name
func (a *InfraProxyServer) DeleteNode(ctx context.Context, r *gwreq.DeleteNode) (*gwres.DeleteNode, error) {

	req := &infra_req.DeleteNode{
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

// UpdateNode updates the node attributes
func (a *InfraProxyServer) UpdateNode(ctx context.Context, r *gwreq.UpdateNode) (*gwres.UpdateNode, error) {
	req := &infra_req.UpdateNode{
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

	return &gwres.UpdateNode{
		Name: res.GetName(),
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
