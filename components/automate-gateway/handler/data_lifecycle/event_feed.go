package datalifecycle

import (
	"context"

	api "github.com/chef/automate/api/external/data_lifecycle"
	"github.com/chef/automate/api/interservice/data_lifecycle"
)

// GetEventFeedStatus returns the event feed purge status
func (s *Server) GetEventFeedStatus(ctx context.Context, req *api.GetEventFeedStatusRequest) (*api.GetEventFeedStatusResponse, error) {
	res := &api.GetEventFeedStatusResponse{}

	purge, err := s.eventFeedPurgeClient.Show(ctx, &data_lifecycle.ShowRequest{})
	if err != nil {
		return res, err
	}

	res.Jobs = append(res.Jobs, purgeShowToJobStatus(purge))

	return res, nil
}

// RunEventFeed runs the event feed data purge
func (s *Server) RunEventFeed(ctx context.Context, req *api.RunEventFeedRequest) (*api.RunEventFeedResponse, error) {
	res := &api.RunEventFeedResponse{}

	_, err := s.eventFeedPurgeClient.Run(ctx, &data_lifecycle.RunRequest{})

	return res, err
}

// SetEventFeedConfig configures the event feed purge policies
func (s *Server) SetEventFeedConfig(ctx context.Context, req *api.SetEventFeedConfigRequest) (*api.SetEventFeedConfigResponse, error) {
	res := &api.SetEventFeedConfigResponse{}

	settings := req.GetJobSettings()
	if settings == nil || len(settings) == 0 {
		return res, nil
	}

	_, err := s.eventFeedPurgeClient.Configure(ctx, jobSettingsToPurgeConfigure(settings[0]))

	return res, err
}
