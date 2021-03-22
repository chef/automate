package grpcserver

import (
	"context"
	"encoding/json"
	"time"

	pRequest "github.com/chef/automate/api/interservice/cfgmgmt/request"
	interserviceResp "github.com/chef/automate/api/interservice/cfgmgmt/response"
	"github.com/golang/protobuf/ptypes"
)

type Datafeed struct {
	FeedStart time.Time
	FeedEnd   time.Time
}

func (s *CfgMgmtServer) Getdata(ctx context.Context, req *pRequest.GetPaginationRequest) (*interserviceResp.GetPaginationResponse, error) {

	params := Datafeed{}
	feedStartString, err := ptypes.TimestampProto(params.FeedStart)
	if err != nil {
		return nil, err
	}
	feedEndString, err := ptypes.TimestampProto(params.FeedEnd)
	if err != nil {
		return nil, err
	}

	nodesRequest := &pRequest.InventoryNodes{
		PageSize: 300,
		Start:    feedStartString,
		End:      feedEndString,
		Sorting: &pRequest.Sorting{
			Order: pRequest.Order_DESC,
		},
	}

	inventoryNodes, err := s.GetInventoryNodes(ctx, nodesRequest)
	if err != nil {
		return nil, err
	}
	out := map[string]interface{}{}
	// data := datafeedServer.paginationData.Data
	out["offset"] = req.GetOffset()
	out["size"] = req.GetSize()
	out["inventoryNodes"] = inventoryNodes
	outputJSON, _ := json.Marshal(out)
	returnData := interserviceResp.GetPaginationResponse{
		Data: string(outputJSON),
	}
	// {Data: string(outputJSON)}
	return &returnData, nil
}

func (s *CfgMgmtServer) FetchCompliancedata(ctx context.Context, req *pRequest.GetPaginationRequest) (*interserviceResp.GetPaginationResponse, error) {

	params := Datafeed{}
	feedStartString, err := ptypes.TimestampProto(params.FeedStart)
	if err != nil {
		return nil, err
	}
	feedEndString, err := ptypes.TimestampProto(params.FeedEnd)
	if err != nil {
		return nil, err
	}

	nodesRequest := &pRequest.InventoryNodes{
		PageSize: req.Size,
		From:     req.Offset,
		Start:    feedStartString,
		End:      feedEndString,
		Sorting: &pRequest.Sorting{
			Order: pRequest.Order_DESC,
		},
	}

	inventoryNodes, err := s.GetInventoryNodes(ctx, nodesRequest)
	if err != nil {
		return nil, err
	}
	out := map[string]interface{}{}
	// data := datafeedServer.paginationData.Data
	out["offset"] = req.Offset
	out["size"] = req.Size
	out["Attribute"] = req.Attribute
	out["inventoryNodes"] = inventoryNodes
	outputJSON, _ := json.Marshal(out)
	returnData := interserviceResp.GetPaginationResponse{
		Data: string(outputJSON),
	}
	// {Data: string(outputJSON)}
	return &returnData, nil
}
