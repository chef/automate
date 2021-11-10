package grpcserver

import (
	"context"
	"time"

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

//GetNodesUsageCount returns the count of unique nodes with lastRun in a given time.
func (s *CfgMgmtServer) GetNodesUsageCount(ctx context.Context, req *request.GetNodesUsageCountRequest) (*response.GetNodesUsageCountResponse, error) {
	var count int64
	// Get last telemetry reported date from postgres
	telemetry, err := s.pg.GetTelemetry(ctx)
	if err != nil {
		return nil, err
	}
	var daysSinceLastPost int
	if telemetry.LastTelemetryReportedAt.IsZero() {
		daysSinceLastPost = 15
	} else {
		daysSinceLastPost = DaysBetween(telemetry.LastTelemetryReportedAt, time.Now())
	}
	if daysSinceLastPost > 0 {
		count, err = s.client.GetUniqueNodesCount(int64(daysSinceLastPost), telemetry.LastTelemetryReportedAt)
		if err != nil {
			return nil, err
		}
	}
	return &response.GetNodesUsageCountResponse{
		DaysSinceLastPost: int64(daysSinceLastPost),
		NodeCnt:           count,
	}, nil
}
