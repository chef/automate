package handler

import (
	"context"
	"fmt"

	"github.com/chef/automate/api/external/report_manager"
	reportManagerService "github.com/chef/automate/api/interservice/report_manager"
	"github.com/chef/automate/components/automate-gateway/protobuf"
	"github.com/golang/protobuf/proto"
	"github.com/golang/protobuf/ptypes/empty"
)

type ReportManager struct {
	client reportManagerService.ReportManagerServiceClient
}

func NewReportManagerHandler(reportManagerClient reportManagerService.ReportManagerServiceClient) *ReportManager {
	return &ReportManager{
		client: reportManagerClient,
	}
}

func (r *ReportManager) ListDownloadReportRequests(ctx context.Context, in *empty.Empty) (*report_manager.ListDownloadReportRequestsResponse, error) {
	requestor := ctx.Value("requestorid")
	if requestor == nil || requestor.(string) == "" {
		return nil, fmt.Errorf("missing requestor info in the context")
	}

	input := &reportManagerService.AllStatusRequest{
		RequestorId: requestor.(string),
	}

	inDomain := &reportManagerService.AllStatusRequest{}
	out := &report_manager.ListDownloadReportRequestsResponse{}
	f := func() (proto.Message, error) {
		return r.client.GetAllRequestsStatus(ctx, inDomain)
	}
	err := protobuf.CallDomainService(input, inDomain, f, out)
	if err != nil {
		return nil, err
	}
	return out, nil
}
