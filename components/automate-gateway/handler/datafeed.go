package handler

import (
	"context"

	"github.com/chef/automate/api/external/data_feed"
	"github.com/chef/automate/components/automate-gateway/protobuf"
	"github.com/golang/protobuf/proto"
)

// Secrets - the data_feed service data structure
type Datafeed struct {
	client data_feed.DatafeedServiceClient
}

// NewDatafeedHandler - create a new datafeed service handler
func NewDatafeedHandler(datafeedClient data_feed.DatafeedServiceClient) *Datafeed {
	return &Datafeed{
		client: datafeedClient,
	}
}

// Create - create a new destination
func (a *Datafeed) AddDestination(ctx context.Context, in *data_feed.AddDestinationRequest) (*data_feed.AddDestinationResponse, error) {
	inDomain := &data_feed.AddDestinationRequest{}
	out := &data_feed.AddDestinationResponse{}
	f := func() (proto.Message, error) {
		return a.client.AddDestination(ctx, inDomain)
	}
	err := protobuf.CallDomainService(in, inDomain, f, out)
	if err != nil {
		return nil, err
	}
	return out, nil
}

func (a *Datafeed) TestDestination(ctx context.Context, in *data_feed.URLValidationRequest) (*data_feed.TestDestinationResponse, error) {
	inDomain := &data_feed.URLValidationRequest{}
	out := &data_feed.TestDestinationResponse{}
	f := func() (proto.Message, error) {
		return a.client.TestDestination(ctx, inDomain)
	}
	err := protobuf.CallDomainService(in, inDomain, f, out)
	if err != nil {
		return nil, err
	}
	return out, nil
}

// GetDestination - get the destination information for one destination.
func (a *Datafeed) GetDestination(ctx context.Context, in *data_feed.GetDestinationRequest) (*data_feed.GetDestinationResponse, error) {
	inDomain := &data_feed.GetDestinationRequest{}
	out := &data_feed.GetDestinationResponse{}
	f := func() (proto.Message, error) {
		return a.client.GetDestination(ctx, inDomain)
	}
	err := protobuf.CallDomainService(in, inDomain, f, out)
	if err != nil {
		return nil, err
	}
	return out, nil
}

// Update - update an existing destination
func (a *Datafeed) UpdateDestination(ctx context.Context, in *data_feed.UpdateDestinationRequest) (*data_feed.UpdateDestinationResponse, error) {
	inDomain := &data_feed.UpdateDestinationRequest{}
	out := &data_feed.UpdateDestinationResponse{}
	f := func() (proto.Message, error) {
		return a.client.UpdateDestination(ctx, inDomain)
	}
	err := protobuf.CallDomainService(in, inDomain, f, out)
	if err != nil {
		return nil, err
	}
	return out, nil
}

// Delete - delete an existing destination
func (a *Datafeed) DeleteDestination(ctx context.Context, in *data_feed.DeleteDestinationRequest) (*data_feed.DeleteDestinationResponse, error) {
	inDomain := &data_feed.DeleteDestinationRequest{}
	out := &data_feed.DeleteDestinationResponse{}
	f := func() (proto.Message, error) {
		return a.client.DeleteDestination(ctx, inDomain)
	}
	err := protobuf.CallDomainService(in, inDomain, f, out)
	if err != nil {
		return nil, err
	}
	return out, nil
}

// List - List the stored destinations
func (a *Datafeed) ListDestinations(ctx context.Context, in *data_feed.ListDestinationRequest) (*data_feed.ListDestinationResponse, error) {
	inDomain := &data_feed.ListDestinationRequest{}
	out := &data_feed.ListDestinationResponse{}
	f := func() (proto.Message, error) {
		return a.client.ListDestinations(ctx, inDomain)
	}
	err := protobuf.CallDomainService(in, inDomain, f, out)
	if err != nil {
		return nil, err
	}
	return out, nil
}

func (a *Datafeed) EnableDestination(ctx context.Context, in *data_feed.UpdateDestinationEnableRequest) (*data_feed.GetDestinationResponse, error) {
	inDomain := &data_feed.UpdateDestinationEnableRequest{}
	out := &data_feed.GetDestinationResponse{}
	f := func() (proto.Message, error) {
		return a.client.EnableDestination(ctx, inDomain)
	}
	err := protobuf.CallDomainService(in, inDomain, f, out)
	if err != nil {
		return nil, err
	}
	return out, nil
}

func (a *Datafeed) GlobalDataFeedConfig(ctx context.Context, in *data_feed.GlobalDataFeedConfigRequest) (*data_feed.GlobalDataFeedConfigResponse, error) {
	inDomain := &data_feed.GlobalDataFeedConfigRequest{}
	out := &data_feed.GlobalDataFeedConfigResponse{}
	f := func() (proto.Message, error) {
		return a.client.GlobalDataFeedConfig(ctx, inDomain)
	}
	err := protobuf.CallDomainService(in, inDomain, f, out)
	if err != nil {
		return nil, err
	}
	return out, nil
}
