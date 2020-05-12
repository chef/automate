package server

import (
	"context"
	"encoding/json"
	"sort"

	chef "github.com/chef/go-chef"
	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/status"

	"github.com/chef/automate/api/interservice/infra_proxy/request"
	"github.com/chef/automate/api/interservice/infra_proxy/response"
)

// GetDataBags get data bags list
func (s *Server) GetDataBags(ctx context.Context, req *request.DataBags) (*response.DataBags, error) {

	c, err := s.createClient(ctx, req.OrgId, req.ServerId)
	if err != nil {
		return nil, status.Errorf(codes.InvalidArgument, "invalid org ID: %s", err.Error())
	}

	if req.Name != "" {
		dataBags, err := c.client.DataBags.ListItems(req.Name)
		if err != nil {
			return nil, status.Error(codes.InvalidArgument, err.Error())
		}

		return &response.DataBags{
			DataBags: fromAPIToListDatabags(*dataBags),
		}, nil

	}

	dataBags, err := c.client.DataBags.List()
	if err != nil {
		return nil, status.Error(codes.InvalidArgument, err.Error())
	}

	return &response.DataBags{
		DataBags: fromAPIToListDatabags(*dataBags),
	}, nil
}

// GetDataBagItem get data bag
func (s *Server) GetDataBagItem(ctx context.Context, req *request.DataBag) (*response.DataBag, error) {
	c, err := s.createClient(ctx, req.OrgId, req.ServerId)
	if err != nil {
		return nil, status.Errorf(codes.InvalidArgument, "invalid org ID: %s", err.Error())
	}

	ic, err := c.client.DataBags.GetItem(req.Name, req.Item)
	if err != nil {
		return nil, status.Error(codes.InvalidArgument, err.Error())
	}

	data, err := json.Marshal(ic)
	if err != nil {
		return nil, status.Error(codes.Internal, err.Error())
	}

	return &response.DataBag{
		Data: string(data),
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
