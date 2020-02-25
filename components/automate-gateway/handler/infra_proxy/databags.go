package infra_proxy

import (
	"context"

	gwreq "github.com/chef/automate/api/external/infra_proxy/request"
	gwres "github.com/chef/automate/api/external/infra_proxy/response"
	infra_req "github.com/chef/automate/api/interservice/infra_proxy/request"
	infra_res "github.com/chef/automate/api/interservice/infra_proxy/response"
)

// GetDataBags fetches an array of existing data bags
func (a *InfraProxyServer) GetDataBags(ctx context.Context, r *gwreq.DataBags) (*gwres.DataBags, error) {
	req := &infra_req.DataBags{
		OrgId: r.OrgId,
	}
	res, err := a.client.GetDataBags(ctx, req)
	if err != nil {
		return nil, err
	}

	return &gwres.DataBags{
		DataBags: fromUpstreamDataBags(res.DataBags),
	}, nil
}

// GetDataBagItem fetch a infra data bag
func (a *InfraProxyServer) GetDataBagItem(ctx context.Context, r *gwreq.DataBag) (*gwres.DataBag, error) {
	req := &infra_req.DataBag{
		OrgId: r.OrgId,
		Name:  r.Name,
	}
	res, err := a.client.GetDataBagItem(ctx, req)
	if err != nil {
		return nil, err
	}

	return &gwres.DataBag{
		Name: res.GetName(),
	}, nil
}

func fromUpstreamDataBags(dbags []*infra_res.DataBagListItem) []*gwres.DataBagListItem {
	ts := make([]*gwres.DataBagListItem, len(dbags))

	for i, c := range dbags {
		ts[i] = &gwres.DataBagListItem{
			Name: c.GetName(),
		}
	}

	return ts
}
