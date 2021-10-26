package grpcserver

import (
	"context"

	"github.com/chef/automate/api/interservice/cfgmgmt/request"
	"github.com/chef/automate/api/interservice/cfgmgmt/response"
	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/status"
)

//UpdateTelemetryReported Update the last client run telemetry reported date in postgres
func (s *CfgMgmtServer) UpdateTelemetryReported(ctx context.Context, req *request.UpdateTelemetryReportedRequest) (*response.UpdateTelemetryReportedResponse, error) {
	if req == nil {
		return nil, status.Error(codes.InvalidArgument, "empty request")
	}

	err := s.pg.UpdateTelemetryReported(ctx, req)
	if err != nil {
		return nil, err
	}

	return &response.UpdateTelemetryReportedResponse{}, nil
}
