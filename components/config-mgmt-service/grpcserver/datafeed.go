package grpcserver

import (
	"context"
	"encoding/json"

	pRequest "github.com/chef/automate/api/interservice/cfgmgmt/request"
	interserviceResp "github.com/chef/automate/api/interservice/cfgmgmt/response"
)

func (s *CfgMgmtServer) Getdata(ctx context.Context, req *pRequest.GetPaginationRequest) (*interserviceResp.GetPaginationResponse, error) {

	out := map[string]interface{}{}
	// data := datafeedServer.paginationData.Data
	out["offset"] = req.GetOffset()
	out["size"] = req.GetSize()

	outputJSON, _ := json.Marshal(out)

	returnData := interserviceResp.GetPaginationResponse{
		Data: string(outputJSON),
	}
	// {Data: string(outputJSON)}
	return &returnData, nil
}
