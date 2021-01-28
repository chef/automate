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

// DataBagItem represents the search based deserialized data bag item
type DataBagItem struct {
	Name        string      `json:"name"`
	RawItemData RawItemData `json:"raw_data"`
}

// RawItemData represents the search based deserialized raw data
type RawItemData struct {
	ID string `json:"id"`
}

// DataBagItemsResult list result from Search API
type DataBagItemsResult struct {
	Total int            `json:"total"`
	Start int            `json:"start"`
	Rows  []*DataBagItem `json:"rows"`
}

// CreateDataBag creates a data bag
func (s *Server) CreateDataBag(ctx context.Context, req *request.CreateDataBag) (*response.CreateDataBag, error) {
	err := validation.New(validation.Options{
		Target:  "databag",
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

	_, err = c.client.DataBags.Create(
		&chef.DataBag{
			Name: req.Name,
		})

	if err != nil {
		return nil, ParseAPIError(err)
	}

	return &response.CreateDataBag{
		Name: req.Name,
	}, nil
}

// CreateDataBagItem creates a data bag item
func (s *Server) CreateDataBagItem(ctx context.Context, req *request.CreateDataBagItem) (*response.CreateDataBagItem, error) {
	err := validation.New(validation.Options{
		Target:  "databag",
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

	dataBagItem, err := StructToJSON(req.Data)
	if err != nil {
		return nil, status.Error(codes.Internal, err.Error())
	}

	err = c.client.DataBags.CreateItem(req.Name, &dataBagItem)
	if err != nil {
		return nil, ParseAPIError(err)
	}

	return &response.CreateDataBagItem{
		Name: req.Name,
	}, nil
}

// GetDataBags gets data bags list
func (s *Server) GetDataBags(ctx context.Context, req *request.DataBags) (*response.DataBags, error) {
	err := validation.New(validation.Options{
		Target:          "databag",
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

	dataBags, err := c.client.DataBags.List()
	if err != nil {
		return nil, ParseAPIError(err)
	}

	return &response.DataBags{
		DataBags: fromAPIToListDatabags(*dataBags),
	}, nil
}

// GetDataBagItems gets data bag items list
func (s *Server) GetDataBagItems(ctx context.Context, req *request.DataBagItems) (*response.DataBagItems, error) {
	c, err := s.createClient(ctx, req.OrgId, req.ServerId)
	if err != nil {
		return nil, err
	}

	if req.SearchQuery == nil || req.SearchQuery.Q == "" {
		res, err := c.client.DataBags.ListItems(req.Name)
		if err != nil {
			return nil, ParseAPIError(err)
		}

		items := fromAPIToListDatabags(*res)
		return &response.DataBagItems{
			Name:  req.Name,
			Items: items,
			Start: 0,
			Total: int32(len(items)),
		}, nil
	}

	res, err := c.SearchDataBagItems(req.Name, req.SearchQuery)
	if err != nil {
		return nil, ParseAPIError(err)
	}

	return &response.DataBagItems{
		Name:  req.Name,
		Items: fromAPIToListDatabagItems(res.Rows),
		Start: int32(res.Start),
		Total: int32(res.Total),
	}, nil
}

// SearchDataBagItems gets data bag items list from Chef Infra Server search API.
func (c *ChefClient) SearchDataBagItems(dataBag string, searchQuery *request.SearchQuery) (DataBagItemsResult, error) {
	var result DataBagItemsResult
	perPage := int(searchQuery.GetRows())
	if perPage == 0 {
		perPage = 1000
	}

	query := chef.SearchQuery{
		Index: dataBag,
		Query: searchQuery.GetQ(),
		Start: int(searchQuery.GetStart()),
		Rows:  perPage,
	}

	fullURL := fmt.Sprintf("search/%s", query)
	newReq, err := c.client.NewRequest("GET", fullURL, nil)

	if err != nil {
		return result, ParseAPIError(err)
	}

	res, err := c.client.Do(newReq, &result)
	if err != nil {
		return result, ParseAPIError(err)
	}

	defer res.Body.Close() // nolint: errcheck

	return result, nil
}

// GetDataBagItem gets data bag item
func (s *Server) GetDataBagItem(ctx context.Context, req *request.DataBagItem) (*response.DataBagItem, error) {
	err := validation.New(validation.Options{
		Target:          "databag",
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

	ic, err := c.client.DataBags.GetItem(req.Name, req.Item)
	if err != nil {
		return nil, ParseAPIError(err)
	}

	data, err := json.Marshal(ic)
	if err != nil {
		return nil, status.Error(codes.Internal, err.Error())
	}

	return &response.DataBagItem{
		Id:   fmt.Sprint(ic.(map[string]interface{})["id"]),
		Name: req.Name,
		Data: string(data),
	}, nil

}

// DeleteDataBag deletes the data bag
func (s *Server) DeleteDataBag(ctx context.Context, req *request.DataBag) (*response.DataBag, error) {
	err := validation.New(validation.Options{
		Target:          "databag",
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

	data, err := c.client.DataBags.Delete(req.Name)
	if err != nil {
		return nil, ParseAPIError(err)
	}

	return &response.DataBag{
		Name: data.Name,
	}, nil

}

// DeleteDataBagItem deletes the data bag item
func (s *Server) DeleteDataBagItem(ctx context.Context, req *request.DataBagItem) (*response.DataBagItem, error) {
	err := validation.New(validation.Options{
		Target:          "databag",
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

	err = c.client.DataBags.DeleteItem(req.Name, req.Item)
	if err != nil {
		return nil, ParseAPIError(err)
	}

	return &response.DataBagItem{
		Name: req.Name,
		Id:   req.Item,
	}, nil

}

// UpdateDataBagItem updates a data bag item
func (s *Server) UpdateDataBagItem(ctx context.Context, req *request.UpdateDataBagItem) (*response.UpdateDataBagItem, error) {
	err := validation.New(validation.Options{
		Target:  "databag",
		Request: *req,
		Rules: validation.Rules{
			"OrgId":    []string{"required"},
			"ServerId": []string{"required"},
			"Name":     []string{"required"},
			"ItemId":   []string{"required"},
		},
	}).Validate()

	if err != nil {
		return nil, err
	}

	c, err := s.createClient(ctx, req.OrgId, req.ServerId)
	if err != nil {
		return nil, err
	}

	dataBagItem, err := StructToJSON(req.Data)
	if err != nil {
		return nil, status.Error(codes.Internal, err.Error())
	}

	err = c.client.DataBags.UpdateItem(req.Name, req.ItemId, &dataBagItem)
	if err != nil {
		return nil, ParseAPIError(err)
	}

	return &response.UpdateDataBagItem{
		Name:   req.Name,
		ItemId: req.ItemId,
	}, nil
}

// fromAPIToListDatabags a response.DataBags from a struct of DataBags
func fromAPIToListDatabags(al chef.DataBagListResult) []*response.DataBagListItem {
	cl := make([]*response.DataBagListItem, len(al))
	index := 0
	for c := range al {
		cl[index] = &response.DataBagListItem{
			Name: c,
		}
		index++
	}

	return cl
}

// fromAPIToListDatabagItems a response data bag items from a struct
func fromAPIToListDatabagItems(al []*DataBagItem) []*response.DataBagListItem {
	cl := make([]*response.DataBagListItem, len(al))
	for index, c := range al {
		cl[index] = &response.DataBagListItem{
			Name: c.RawItemData.ID,
		}
	}

	return cl
}
