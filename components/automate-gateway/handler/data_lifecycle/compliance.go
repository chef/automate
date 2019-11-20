package datalifecycle

import (
	"context"

	api "github.com/chef/automate/api/external/data_lifecycle"
	"github.com/chef/automate/api/interservice/data_lifecycle"
)

// GetComplianceStatus returns the compliance purge status
func (s *Server) GetComplianceStatus(ctx context.Context, req *api.GetComplianceStatusRequest) (*api.GetComplianceStatusResponse, error) {
	res := &api.GetComplianceStatusResponse{}

	purge, err := s.compliancePurgeClient.Show(ctx, &data_lifecycle.ShowRequest{})
	if err != nil {
		return res, err
	}

	res.Jobs = append(res.Jobs, purgeShowToJobStatus(purge))

	return res, nil
}

// RunCompliance runs the compliance data purge
func (s *Server) RunCompliance(ctx context.Context, req *api.RunComplianceRequest) (*api.RunComplianceResponse, error) {
	res := &api.RunComplianceResponse{}

	_, err := s.compliancePurgeClient.Run(ctx, &data_lifecycle.RunRequest{})

	return res, err
}

// SetComplianceConfig configures the compliance purge policies
func (s *Server) SetComplianceConfig(ctx context.Context, req *api.SetComplianceConfigRequest) (*api.SetComplianceConfigResponse, error) {
	res := &api.SetComplianceConfigResponse{}

	settings := req.GetJobSettings()
	if settings == nil || len(settings) == 0 {
		return res, nil
	}

	_, err := s.compliancePurgeClient.Configure(ctx, jobSettingsToPurgeConfigure(settings[0]))

	return res, err
}
