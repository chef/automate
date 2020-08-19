package datalifecycle

import (
	"context"
	"time"

	apps "github.com/chef/automate/api/external/applications"
	api "github.com/chef/automate/api/external/data_lifecycle"
	"github.com/chef/automate/api/interservice/data_lifecycle"
	"github.com/chef/automate/api/interservice/ingest"
	"github.com/golang/protobuf/ptypes"
	"github.com/golang/protobuf/ptypes/duration"
	"github.com/golang/protobuf/ptypes/timestamp"
)

// NewServer returns a pointer to a new instance of the Server
func NewServer(ingestJobClient ingest.JobSchedulerServiceClient,
	ingestPurgeClient data_lifecycle.PurgeClient,
	compliancePurgeClient data_lifecycle.PurgeClient,
	eventFeedPurgeClient data_lifecycle.PurgeClient,
	appsClient apps.ApplicationsServiceClient,
) *Server {

	return &Server{
		ingestJobClient:       ingestJobClient,
		ingestPurgeClient:     ingestPurgeClient,
		compliancePurgeClient: compliancePurgeClient,
		eventFeedPurgeClient:  eventFeedPurgeClient,
		appsClient:            appsClient,
	}
}

// Server is the data lifecycle server handler implementation
type Server struct {
	ingestJobClient       ingest.JobSchedulerServiceClient
	ingestPurgeClient     data_lifecycle.PurgeClient
	compliancePurgeClient data_lifecycle.PurgeClient
	eventFeedPurgeClient  data_lifecycle.PurgeClient
	appsClient            apps.ApplicationsServiceClient
}

// NOTE: services is currently unimplemented as the API's are not stable

// GetStatus returns the aggregate status across all data lifecycle schedules
// and configuration.
func (s *Server) GetStatus(ctx context.Context, req *api.GetStatusRequest) (*api.GetStatusResponse, error) {
	res := &api.GetStatusResponse{}
	var err error

	res.Infra, err = s.GetInfraStatus(ctx, &api.GetInfraStatusRequest{})
	if err != nil {
		return res, err
	}

	res.Compliance, err = s.GetComplianceStatus(ctx, &api.GetComplianceStatusRequest{})
	if err != nil {
		return res, err
	}

	res.EventFeed, err = s.GetEventFeedStatus(ctx, &api.GetEventFeedStatusRequest{})
	if err != nil {
		return res, err
	}

	res.Services, err = s.GetServicesStatus(ctx, &api.GetServicesStatusRequest{})
	if err != nil {
		return res, err
	}

	return res, nil
}

// SetConfig configures all data lifecycle jobs
func (s *Server) SetConfig(ctx context.Context, req *api.SetConfigRequest) (*api.SetConfigResponse, error) {
	res := &api.SetConfigResponse{}
	var err error

	if infra := req.GetInfra(); infra != nil {
		_, err = s.SetInfraConfig(ctx, infra)
		if err != nil {
			return res, err
		}
	}

	if compliance := req.GetCompliance(); compliance != nil {
		_, err = s.SetComplianceConfig(ctx, compliance)
		if err != nil {
			return res, err
		}
	}

	if eventFeed := req.GetEventFeed(); eventFeed != nil {
		_, err = s.SetEventFeedConfig(ctx, eventFeed)
		if err != nil {
			return res, err
		}
	}

	if svcs := req.GetServices(); svcs != nil {
		_, err = s.SetServicesConfig(ctx, svcs)
		if err != nil {
			return res, err
		}
	}

	return res, nil
}

// Run runs all data lifecycle jobs
func (s *Server) Run(ctx context.Context, req *api.RunRequest) (*api.RunResponse, error) {
	res := &api.RunResponse{}
	var err error

	_, err = s.RunInfra(ctx, &api.RunInfraRequest{})
	if err != nil {
		return res, err
	}

	_, err = s.RunCompliance(ctx, &api.RunComplianceRequest{})
	if err != nil {
		return res, err
	}

	_, err = s.RunEventFeed(ctx, &api.RunEventFeedRequest{})
	if err != nil {
		return res, err
	}

	_, err = s.RunServices(ctx, &api.RunServicesRequest{})
	if err != nil {
		return res, err
	}

	return res, nil
}

func purgeShowToJobStatus(purge *data_lifecycle.ShowResponse) *api.JobStatus {
	return &api.JobStatus{
		Name:           purge.InstanceName,
		Disabled:       !purge.Enabled,
		Recurrence:     purge.Recurrence,
		NextDueAt:      purge.NextDueAt,
		LastEnqueuedAt: purge.LastEnqueuedAt,
		LastStartedAt:  purge.LastStart,
		LastEndedAt:    purge.LastEnd,
		LastElapsed:    maybeLastElapsed(purge.LastStart, purge.LastEnd),
		PurgePolicies: &api.PurgePolicies{
			Elasticsearch: purge.EsPolicies,
			Postgres:      purge.PgPolicies,
		},
	}
}

func jobSettingsToPurgeConfigure(setting *api.JobSettings) *data_lifecycle.ConfigureRequest {
	return &data_lifecycle.ConfigureRequest{
		Enabled:    !setting.Disabled,
		Recurrence: setting.Recurrence,
		PolicyUpdate: &data_lifecycle.PolicyUpdate{
			Es: setting.GetPurgePolicies().GetElasticsearch(),
			Pg: setting.GetPurgePolicies().GetPostgres(),
		},
	}
}

func maybeToTimestamp(timestamp string) *timestamp.Timestamp {
	t, err := time.Parse(time.RFC3339Nano, timestamp)
	if err != nil {
		return nil
	}

	tproto, err := ptypes.TimestampProto(t)
	if err != nil {
		return nil
	}

	return tproto
}

func maybeToDuration(duration string) *duration.Duration {
	d, err := time.ParseDuration(duration)
	if err != nil {
		return nil
	}

	return ptypes.DurationProto(d)
}

func maybeToLastEnded(start *timestamp.Timestamp, took *duration.Duration) *timestamp.Timestamp {
	if start == nil || took == nil {
		return nil
	}

	started, err := ptypes.Timestamp(start)
	if err != nil {
		return nil
	}

	elapsed, err := ptypes.Duration(took)
	if err != nil {
		return nil
	}

	ended, err := ptypes.TimestampProto(started.Add(elapsed))
	if err != nil {
		return nil
	}

	return ended
}

func maybeLastElapsed(start *timestamp.Timestamp, end *timestamp.Timestamp) *duration.Duration {
	if start == nil || end == nil {
		return nil
	}

	started, err := ptypes.Timestamp(start)
	if err != nil {
		return nil
	}

	ended, err := ptypes.Timestamp(end)
	if err != nil {
		return nil
	}

	return ptypes.DurationProto(ended.Sub(started))
}
