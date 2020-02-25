package v1

import (
	"context"
	"fmt"
	"sort"

	chef "github.com/chef/go-chef"
	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/status"

	"github.com/chef/automate/api/interservice/infra_proxy/request"
	"github.com/chef/automate/api/interservice/infra_proxy/response"
)

// GetDataBags get data bags list
func (s *Server) GetDataBags(ctx context.Context, req *request.DataBags) (*response.DataBags, error) {

	client, err := s.createClient(ctx, req.OrgId)
	if err != nil {
		return nil, status.Errorf(codes.InvalidArgument, "invalid org id: %s", err.Error())
	}

	if req.Name != "" {
		dataBags, err := client.DataBags.ListItems(req.Name)

		if err != nil {
			return nil, status.Error(codes.InvalidArgument, err.Error())
		}

		return &response.DataBags{
			DataBags: fromAPIToListDatabags(*dataBags),
		}, nil

	} else {

		dataBags, err := client.DataBags.List()

		if err != nil {
			return nil, status.Error(codes.InvalidArgument, err.Error())
		}

		return &response.DataBags{
			DataBags: fromAPIToListDatabags(*dataBags),
		}, nil
	}
}

// GetDataBagItem get data bag
func (s *Server) GetDataBagItem(ctx context.Context, req *request.DataBag) (*response.DataBag, error) {
	client, err := s.createClient(ctx, req.OrgId)
	if err != nil {
		return nil, status.Errorf(codes.InvalidArgument, "invalid org id: %s", err.Error())
	}

	ic, err := client.DataBags.GetItem(req.Name, req.Item)
	if err != nil {
		return nil, status.Error(codes.InvalidArgument, err.Error())
	}

	return &response.DataBag{
		Name: fmt.Sprintf("%v", ic),
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
