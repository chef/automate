package infra_proxy

import (
	"context"

	gwreq "github.com/chef/automate/api/external/infra_proxy/request"
	gwres "github.com/chef/automate/api/external/infra_proxy/response"
	infra_req "github.com/chef/automate/api/interservice/infra_proxy/request"
	infra_res "github.com/chef/automate/api/interservice/infra_proxy/response"
)

// CreateDataBag creates a data bag
func (a *InfraProxyServer) CreateDataBag(ctx context.Context, r *gwreq.CreateDataBag) (*gwres.CreateDataBag, error) {
	req := &infra_req.CreateDataBag{
		OrgId:    r.OrgId,
		ServerId: r.ServerId,
		Name:     r.Name,
	}
	res, err := a.client.CreateDataBag(ctx, req)
	if err != nil {
		return nil, err
	}

	return &gwres.CreateDataBag{
		Name: res.GetName(),
	}, nil
}

// GetDataBags fetches an array of existing data bags
func (a *InfraProxyServer) GetDataBags(ctx context.Context, r *gwreq.DataBags) (*gwres.DataBags, error) {
	req := &infra_req.DataBags{
		OrgId:    r.OrgId,
		ServerId: r.ServerId,
	}
	res, err := a.client.GetDataBags(ctx, req)
	if err != nil {
		return nil, err
	}

	return &gwres.DataBags{
		DataBags: fromUpstreamDataBags(res.DataBags),
	}, nil
}

// GetDataBagItem fetches an infra data bag item details
func (a *InfraProxyServer) GetDataBagItem(ctx context.Context, r *gwreq.DataBag) (*gwres.DataBag, error) {
	req := &infra_req.DataBag{
		OrgId:    r.OrgId,
		ServerId: r.ServerId,
		Name:     r.Name,
		Item:     r.Item,
	}
	res, err := a.client.GetDataBagItem(ctx, req)
	if err != nil {
		return nil, err
	}

	return &gwres.DataBag{
		Id:   res.GetId(),
		Name: res.GetName(),
		Data: res.GetData(),
	}, nil
}

// DeleteDataBag deletes the data bag and data bag item
func (a *InfraProxyServer) DeleteDataBag(ctx context.Context, r *gwreq.DataBag) (*gwres.DataBag, error) {
	req := &infra_req.DataBag{
		OrgId:    r.OrgId,
		ServerId: r.ServerId,
		Name:     r.Name,
		Item:     r.Item,
	}
	res, err := a.client.DeleteDataBag(ctx, req)
	if err != nil {
		return nil, err
	}

	return &gwres.DataBag{
		Id:   res.GetId(),
		Name: res.GetName(),
		Data: res.GetData(),
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
