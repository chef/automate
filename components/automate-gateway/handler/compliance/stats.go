package compliance

import (
	"context"
	"errors"

	"github.com/chef/automate/api/external/compliance/reporting/stats"
	statsService "github.com/chef/automate/api/interservice/compliance/stats"
	"github.com/chef/automate/components/automate-gateway/protobuf"
	"github.com/golang/protobuf/proto"
)

type Stats struct {
	client statsService.StatsServiceClient
}

func NewStatsHandler(statsClient statsService.StatsServiceClient) *Stats {
	return &Stats{
		client: statsClient,
	}
}

// should cover /summary, /summary/nodes, /summary/controls
func (a *Stats) ReadSummary(ctx context.Context, in *stats.Query) (*stats.Summary, error) {
	inDomain := &statsService.Query{}
	out := &stats.Summary{}
	f := func() (proto.Message, error) {
		return a.client.ReadSummary(ctx, inDomain)
	}
	err := protobuf.CallDomainService(in, inDomain, f, out)
	if err != nil {
		return nil, err
	}
	return out, nil
}

// should cover /trend/nodes, /trend/controls
func (a *Stats) ReadTrend(ctx context.Context, in *stats.Query) (*stats.Trends, error) {
	inDomain := &statsService.Query{}
	out := &stats.Trends{}
	f := func() (proto.Message, error) {
		return a.client.ReadTrend(ctx, inDomain)
	}
	err := protobuf.CallDomainService(in, inDomain, f, out)
	if err != nil {
		return nil, err
	}
	return out, nil
}

// should cover /profiles, profiles/:profile-id/summary, profiles/:profile-id/controls
func (a *Stats) ReadProfiles(ctx context.Context, in *stats.Query) (*stats.Profile, error) {
	inDomain := &statsService.Query{}
	out := &stats.Profile{}
	f := func() (proto.Message, error) {
		return a.client.ReadProfiles(ctx, inDomain)
	}
	err := protobuf.CallDomainService(in, inDomain, f, out)
	if err != nil {
		return nil, err
	}
	return out, nil
}

func (a *Stats) ReadFailures(ctx context.Context, in *stats.Query) (*stats.Failures, error) {
	inDomain := &statsService.Query{}
	out := &stats.Failures{}
	f := func() (proto.Message, error) {
		return a.client.ReadFailures(ctx, inDomain)
	}
	err := protobuf.CallDomainService(in, inDomain, f, out)
	if err != nil {
		return nil, err
	}
	return out, nil
}

//UpdateTelemetryReported Updates the last compliance telemetry reported date after the telemetry data is sent
func (a *Stats) UpdateTelemetryReported(ctx context.Context, in *stats.UpdateTelemetryReportedRequest) (*stats.UpdateTelemetryReportedResponse, error) {
	inDomain := &statsService.UpdateTelemetryReportedRequest{
		LastTelemetryReportedAt: in.LastTelemetryReportedAt,
	}
	if in.LastTelemetryReportedAt == "" {
		return nil, errors.New("LastTelemetryReported timestamp is required")
	}
	out := &stats.UpdateTelemetryReportedResponse{}
	f := func() (proto.Message, error) {
		return a.client.UpdateTelemetryReported(ctx, inDomain)
	}
	err := protobuf.CallDomainService(in, inDomain, f, out)
	if err != nil {
		return nil, err
	}
	return out, nil
}

//GetNodesUsageCount returns the count of unique nodes with lastRun in a given time.
func (a *Stats) GetNodesUsageCount(ctx context.Context, in *stats.GetNodesUsageCountRequest) (*stats.GetNodesUsageCountResponse, error) {
	inDomain := &statsService.GetNodesUsageCountRequest{}
	out := &stats.GetNodesUsageCountResponse{}
	f := func() (proto.Message, error) {
		return a.client.GetNodesUsageCount(ctx, inDomain)
	}
	err := protobuf.CallDomainService(in, inDomain, f, out)
	if err != nil {
		return nil, err
	}
	return out, nil
}
