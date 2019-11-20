package datalifecycle

import (
	"context"

	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/status"

	api "github.com/chef/automate/api/external/data_lifecycle"
)

// The applications service is still under the beta flag. We'll leave these
// unimplemented until those APIs become stable and we can integrate them.

// GetServicesStatus returns the applications data lifecycle status
func (s *Server) GetServicesStatus(ctx context.Context, req *api.GetServicesStatusRequest) (*api.GetServicesStatusResponse, error) {
	return nil, status.Errorf(codes.Unimplemented, "method GetServicesStatus not implemented")
}

// RunServices runs the applications data life cycle jobs
func (s *Server) RunServices(ctx context.Context, req *api.RunServicesRequest) (*api.RunServicesResponse, error) {
	return nil, status.Errorf(codes.Unimplemented, "method RunServices not implemented")
}

// SetServicesConfig configures the applications data lifecycle jobs
func (s *Server) SetServicesConfig(ctx context.Context, req *api.SetServicesConfigRequest) (*api.SetServicesConfigResponse, error) {
	return nil, status.Errorf(codes.Unimplemented, "method SetServicesConfig not implemented")
}
