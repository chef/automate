package server

import (
	"context"

	"github.com/chef/automate/api/external/cds/request"
	"github.com/chef/automate/api/external/cds/response"
	ver_api "github.com/chef/automate/api/external/common/version"

	"github.com/chef/automate/components/automate-cds/service"
	"github.com/chef/automate/lib/version"
	log "github.com/sirupsen/logrus"
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

// ListContentItems - Returns a list of CDS content metadata
func (s *Server) ListContentItems(ctx context.Context, request *request.ContentItems) (*response.ContentItems, error) {

	// Canned data
	return &response.ContentItems{
		Items: []*response.ContentItem{
			{
				Id:             "f354577e-2ea5-4c96-86ed-78a8ba68e0f3",
				Name:           "DevSec Linux Security Baseline Benchmark Level 1",
				Description:    "Test suite for best practice Linux OS hardening",
				Type:           "profile",
				Version:        "1.9.0",
				Platforms:      []string{"Ubuntu", "Debian"},
				CanBeInstalled: true,
			},
			{
				Id:             "1f9e6a1e-3381-487f-98ef-9966c65ff92a",
				Name:           "Audit Benchmark level 1",
				Description:    "Test suite for best practice Linux OS hardening",
				Type:           "cookbook",
				Version:        "9.2.1",
				Platforms:      []string{"Redhat", "Centos"},
				CanBeInstalled: false,
			},
			{
				Id:             "736f5724-b384-4d74-b8cb-dedd9f1ecd54",
				Name:           "Compliance Effortless Package",
				Description:    "This is a hart file that enables effortless compliance on your fleet.",
				Type:           "package",
				Version:        "2.3.2",
				Platforms:      []string{"Solaris"},
				CanBeInstalled: true,
			},
		},
	}, nil
}

// InstallContentItem - installing a content item
func (s *Server) InstallContentItem(ctx context.Context, request *request.InstallContentItem) (*response.InstallContentItem, error) {

	log.Infof("Downloading content item with ID %s ...", request.Id)

	log.Infof("Installing content item with ID %s ...", request.Id)

	return &response.InstallContentItem{}, nil
}
