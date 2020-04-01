package infra_proxy

import (
	"context"

	gwreq "github.com/chef/automate/api/external/infra_proxy/request"
	gwres "github.com/chef/automate/api/external/infra_proxy/response"
	infra_req "github.com/chef/automate/api/interservice/infra_proxy/request"
	infra_res "github.com/chef/automate/api/interservice/infra_proxy/response"
)

// GetAffectedNodes get the nodes using resource
func (a *InfraProxyServer) GetAffectedNodes(ctx context.Context, r *gwreq.AffectedNodes) (*gwres.AffectedNodes, error) {

	req := &infra_req.AffectedNodes{
		OrgId:    r.OrgId,
		Resource: r.Resource,
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

func parseNodeAttributeFromRes(nodes []*infra_res.NodeAttribute) []*gwres.NodeAttribute {
	nl := make([]*gwres.NodeAttribute, len(nodes))

	for i, node := range nodes {
		nl[i] = &gwres.NodeAttribute{
			Name:        node.Name,
			CheckIn:     node.CheckIn,
			ChefGuid:    node.ChefGuid,
			Environment: node.Environment,
			Platform:    node.Platform,
			PolicyGroup: node.PolicyGroup,
			Uptime:      node.Uptime,
		}
	}

	return nl
}
