package server

import (
	"context"

	"github.com/chef/automate/api/external/cds/request"
	"github.com/chef/automate/api/external/cds/response"
	ver_api "github.com/chef/automate/api/external/common/version"

	"github.com/chef/automate/components/automate-cds/service"
	"github.com/chef/automate/lib/version"
)

// Server is an Automate CDS proxy server
type Server struct {
	service *service.Service
}

// NewServer returns an Automate CDS proxy server
func NewServer(service *service.Service) *Server {
	return &Server{service: service}
}

// GetVersion returns the version of Automate CDS GRPC API
func (s *Server) GetVersion(
	ctx context.Context,
	_ *ver_api.VersionInfoRequest) (*ver_api.VersionInfo, error) {
	return &ver_api.VersionInfo{
		Name:    "automate-cds",
		Version: version.Version,
		Sha:     version.GitSHA,
		Built:   version.BuildTime,
	}, nil
}

// GetContentItems - Returns a list of CDS content metadata
func (s *Server) GetContentItems(ctx context.Context, request *request.ContentItems) (*response.ContentItems, error) {

	return &response.ContentItems{}, nil
}
