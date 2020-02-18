package v1

import (
	"context"

	ver_api "github.com/chef/automate/api/external/common/version"
	"github.com/chef/automate/components/infra-proxy-service/service"
	"github.com/chef/automate/lib/version"
)

// Server is a V1 infra-proxy server
type Server struct {
	service *service.Service
}

// NewServer returns a V1 infra-proxy server
func NewServer(service *service.Service) *Server {
	return &Server{service: service}
}

// GetVersion returns the version of InfraProxy GRPC API
func (s *Server) GetVersion(
	ctx context.Context,
	_ *ver_api.VersionInfoRequest) (*ver_api.VersionInfo, error) {
	return &ver_api.VersionInfo{
		Name:    "infra-proxy-service",
		Version: version.Version,
		Sha:     version.GitSHA,
		Built:   version.BuildTime,
	}, nil
}
