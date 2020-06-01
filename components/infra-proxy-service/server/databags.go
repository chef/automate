package server

import (
	"context"
	"encoding/json"
	"fmt"
	"sort"

	chef "github.com/go-chef/chef"
	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/status"

	"github.com/chef/automate/api/interservice/infra_proxy/request"
	"github.com/chef/automate/api/interservice/infra_proxy/response"
)

// CreateDataBag creates a data bag
func (s *Server) CreateDataBag(ctx context.Context, req *request.CreateDataBag) (*response.CreateDataBag, error) {
	c, err := s.createClient(ctx, req.OrgId, req.ServerId)
	if err != nil {
		return nil, err
	}

	if req.Name == "" {
		return nil, status.Error(codes.InvalidArgument, "must supply data bag name")
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
	c, err := s.createClient(ctx, req.OrgId, req.ServerId)
	if err != nil {
		return nil, err
	}

	if req.Name == "" {
		return nil, status.Error(codes.InvalidArgument, "must supply data bag name")
	}

	var dataBagItem chef.DataBagItem
	err = json.Unmarshal(json.RawMessage(req.Data), &dataBagItem)
	if err != nil {
		return nil, status.Error(codes.Internal, err.Error())
	}

	err = c.client.DataBags.CreateItem(req.Name, dataBagItem)
	if err != nil {
		return nil, ParseAPIError(err)
	}

	return &response.CreateDataBagItem{
		Name: req.Name,
	}, nil
}

// GetDataBags get data bags list
func (s *Server) GetDataBags(ctx context.Context, req *request.DataBags) (*response.DataBags, error) {
	c, err := s.createClient(ctx, req.OrgId, req.ServerId)
	if err != nil {
		return nil, err
	}

	if req.Name != "" {
		dataBags, err := c.client.DataBags.ListItems(req.Name)
		if err != nil {
			return nil, ParseAPIError(err)
		}

		return &response.DataBags{
			DataBags: fromAPIToListDatabags(*dataBags),
		}, nil

	}

	dataBags, err := c.client.DataBags.List()
	if err != nil {
		return nil, ParseAPIError(err)
	}

	return &response.DataBags{
		DataBags: fromAPIToListDatabags(*dataBags),
	}, nil
}

// GetDataBagItem get data bag
func (s *Server) GetDataBagItem(ctx context.Context, req *request.DataBag) (*response.DataBag, error) {
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

	return &response.DataBag{
		Id:   fmt.Sprint(ic.(map[string]interface{})["id"]),
		Name: req.Name,
		Data: string(data),
	}, nil

}

// DeleteDataBag delete the data bag and data bag item
func (s *Server) DeleteDataBag(ctx context.Context, req *request.DataBag) (*response.DataBag, error) {
	c, err := s.createClient(ctx, req.OrgId, req.ServerId)
	if err != nil {
		return nil, err
	}

	if req.Name == "" {
		return nil, status.Error(codes.InvalidArgument, "must supply data bag name")
	}

	if req.Item != "" {
		err = c.client.DataBags.DeleteItem(req.Name, req.Item)
		if err != nil {
			return nil, status.Error(codes.Internal, err.Error())
		}

		return &response.DataBag{
			Id:   req.Item,
			Name: req.Name,
		}, nil
	}

	data, err := c.client.DataBags.Delete(req.Name)
	if err != nil {
		return nil, ParseAPIError(err)
	}

	return &response.DataBag{
		Name: data.Name,
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

	sort.Slice(cl, func(i, j int) bool {
		return cl[i].Name < cl[j].Name
	})

	return cl
}
