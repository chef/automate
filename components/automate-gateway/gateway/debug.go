package gateway

import (
	"context"

	"github.com/chef/automate/lib/grpc/debug"
	"github.com/chef/automate/lib/grpc/debug/debug_api"
)

type debugServer struct {
	impl debug_api.DebugServer
}

var _ debug_api.DebugServer = (*debugServer)(nil)

func NewDebugServer() *debugServer {
	s := debug.NewDebugServer()
	return &debugServer{
		impl: s,
	}
}

func (s *debugServer) Trace(req *debug_api.TraceRequest, resp debug_api.Debug_TraceServer) error {
	return s.impl.Trace(req, resp)
}

func (s *debugServer) Profile(req *debug_api.ProfileRequest, resp debug_api.Debug_ProfileServer) error {
	return s.impl.Profile(req, resp)
}

func (s *debugServer) SetLogLevel(ctx context.Context, req *debug_api.SetLogLevelRequest) (*debug_api.SetLogLevelResponse, error) {
	return s.impl.SetLogLevel(ctx, req)
}

func (s *debugServer) GetVersion(ctx context.Context, req *debug_api.VersionRequest) (*debug_api.VersionResponse, error) {
	return s.impl.GetVersion(ctx, req)
}

func (s *debugServer) MustUseDeploymentCertAuth() {}
