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

// GetDataBagItems fetches an array of existing data bag items
func (a *InfraProxyServer) GetDataBagItems(ctx context.Context, r *gwreq.DataBagItems) (*gwres.DataBagItems, error) {
	req := &infra_req.DataBagItems{
		OrgId:    r.OrgId,
		ServerId: r.ServerId,
		Name:     r.Name,
		SearchQuery: &infra_req.SearchQuery{
			Q:       r.GetSearchQuery().GetQ(),
			PerPage: r.GetSearchQuery().GetPerPage(),
			Page:    r.GetSearchQuery().GetPage(),
		},
	}
	res, err := a.client.GetDataBagItems(ctx, req)
	if err != nil {
		return nil, err
	}

	return &gwres.DataBagItems{
		Name:  res.GetName(),
		Items: fromUpstreamDataBags(res.Items),
		Page:  res.GetPage(),
		Total: res.GetTotal(),
	}, nil
}

// GetDataBagItem fetches an infra data bag item details
func (a *InfraProxyServer) GetDataBagItem(ctx context.Context, r *gwreq.DataBagItem) (*gwres.DataBagItem, error) {
	req := &infra_req.DataBagItem{
		OrgId:    r.OrgId,
		ServerId: r.ServerId,
		Name:     r.Name,
		Item:     r.Item,
	}
	res, err := a.client.GetDataBagItem(ctx, req)
	if err != nil {
		return nil, err
	}

	return &gwres.DataBagItem{
		Id:   res.GetId(),
		Name: res.GetName(),
		Data: res.GetData(),
	}, nil
}

// DeleteDataBag deletes the data bag
func (a *InfraProxyServer) DeleteDataBag(ctx context.Context, r *gwreq.DataBag) (*gwres.DataBag, error) {
	req := &infra_req.DataBag{
		OrgId:    r.OrgId,
		ServerId: r.ServerId,
		Name:     r.Name,
	}
	res, err := a.client.DeleteDataBag(ctx, req)
	if err != nil {
		return nil, err
	}

	return &gwres.DataBag{
		Name: res.GetName(),
	}, nil
}

// DeleteDataBagItem deletes the data bag item
func (a *InfraProxyServer) DeleteDataBagItem(ctx context.Context, r *gwreq.DataBagItem) (*gwres.DataBagItem, error) {
	req := &infra_req.DataBagItem{
		OrgId:    r.OrgId,
		ServerId: r.ServerId,
		Name:     r.Name,
		Item:     r.Item,
	}
	res, err := a.client.DeleteDataBagItem(ctx, req)
	if err != nil {
		return nil, err
	}

	return &gwres.DataBagItem{
		Id:   res.GetId(),
		Name: res.GetName(),
		Data: res.GetData(),
	}, nil
}

// CreateDataBagItem creates a data bag item
func (a *InfraProxyServer) CreateDataBagItem(ctx context.Context, r *gwreq.CreateDataBagItem) (*gwres.CreateDataBagItem, error) {
	req := &infra_req.CreateDataBagItem{
		OrgId:    r.OrgId,
		ServerId: r.ServerId,
		Name:     r.Name,
		Data:     r.Data,
	}
	res, err := a.client.CreateDataBagItem(ctx, req)
	if err != nil {
		return nil, err
	}

	return &gwres.CreateDataBagItem{
		Name: res.GetName(),
		Id:   res.GetId(),
	}, nil
}

// UpdateDataBagItem updates a data bag item
func (a *InfraProxyServer) UpdateDataBagItem(ctx context.Context, r *gwreq.UpdateDataBagItem) (*gwres.UpdateDataBagItem, error) {
	req := &infra_req.UpdateDataBagItem{
		OrgId:    r.OrgId,
		ServerId: r.ServerId,
		Name:     r.Name,
		ItemId:   r.ItemId,
		Data:     r.Data,
	}
	res, err := a.client.UpdateDataBagItem(ctx, req)
	if err != nil {
		return nil, err
	}

	return &gwres.UpdateDataBagItem{
		Name:   res.GetName(),
		ItemId: res.GetItemId(),
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
