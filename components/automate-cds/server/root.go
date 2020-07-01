package server

import (
	"bufio"
	"context"
	"fmt"

	"github.com/chef/automate/api/external/cds/request"
	"github.com/chef/automate/api/external/cds/response"
	"github.com/chef/automate/api/external/common"
	ver_api "github.com/chef/automate/api/external/common/version"
	interservice "github.com/chef/automate/api/interservice/cds/service"

	"github.com/chef/automate/components/automate-cds/service"
	"github.com/chef/automate/lib/io/chunks"
	"github.com/chef/automate/lib/version"
	log "github.com/sirupsen/logrus"
	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/status"

	"net/http"

	profiles "github.com/chef/automate/api/interservice/compliance/profiles"
)

// Chosen somewhat arbitrarily to be a "good enough" value.
// See: https://github.com/chef/automate/pull/1143#discussion_r170428374
const streamBufferSize = 262144

// Server is an Automate CDS proxy server
type Server struct {
	service               *service.Service
	profilesServiceClient profiles.ProfilesServiceClient
}

// NewServer returns an Automate CDS proxy server
func NewServer(service *service.Service, profilesServiceClient profiles.ProfilesServiceClient) *Server {
	return &Server{
		service:               service,
		profilesServiceClient: profilesServiceClient,
	}
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
				Filename:       "linux-baseline-2.2.2.tar.gz",
			},
			{
				Id:             "1f9e6a1e-3381-487f-98ef-9966c65ff92a",
				Name:           "Audit Benchmark level 1",
				Description:    "Test suite for best practice Linux OS hardening",
				Type:           "cookbook",
				Version:        "9.2.1",
				Platforms:      []string{"Redhat", "Centos"},
				CanBeInstalled: false,
				Filename:       "audit-benchmark-level-1.tar.gz",
			},
			{
				Id:             "736f5724-b384-4d74-b8cb-dedd9f1ecd54",
				Name:           "Compliance Effortless Package",
				Description:    "This is a hart file that enables effortless compliance on your fleet.",
				Type:           "package",
				Version:        "2.3.2",
				Platforms:      []string{"Solaris"},
				CanBeInstalled: false,
				Filename:       "chef-compliance-effortless-1.1.1-20200626161151-x86_64-linux.hart",
			},
		},
	}, nil
}

// InstallContentItem - installing a content item
func (s *Server) InstallContentItem(ctx context.Context, request *request.InstallContentItem) (*response.InstallContentItem, error) {

	url := getSaaSURL(request.Id)
	log.Infof("Downloading content item with ID %s and URL: %s", request.Id, url)

	// Cookbook and Package types can not be installed
	if request.Id == "736f5724-b384-4d74-b8cb-dedd9f1ecd54" ||
		request.Id == "1f9e6a1e-3381-487f-98ef-9966c65ff92a" {
		// return an error

		return &response.InstallContentItem{}, status.Errorf(codes.InvalidArgument, "Can not be installed")
	}

	if request.Id == "f354577e-2ea5-4c96-86ed-78a8ba68e0f3" {
		log.Infof("Installing content item with ID %s ...", request.Id)

		s.profilesServiceClient.Create()
		// stream the file from the SaaS to the compliance-service
	}

	return &response.InstallContentItem{}, nil
}

// DownloadContentItem - download content item
func (s *Server) DownloadContentItem(request *request.DownloadContentItem,
	stream interservice.AutomateCds_DownloadContentItemServer) error {

	log.Infof("DownloadContentItem: Downloading content item with ID %s ...", request.Id)

	url := getSaaSURL(request.Id)

	// Get the data
	resp, err := http.Get(url)
	if err != nil {
		return err
	}
	defer resp.Body.Close()

	writer := chunks.NewWriter(streamBufferSize, func(p []byte) error {
		return stream.Send(&common.ExportData{Content: p})
	})

	reader := bufio.NewReaderSize(resp.Body, streamBufferSize)

	_, err = reader.WriteTo(writer)
	if err != nil {
		return fmt.Errorf("Failed to stream: %+v", err)
	}

	return nil
}

func getSaaSURL(id string) string {
	if id == "f354577e-2ea5-4c96-86ed-78a8ba68e0f3" {
		return "https://github.com/dev-sec/apache-baseline/archive/master.tar.gz"
	}

	if id == "736f5724-b384-4d74-b8cb-dedd9f1ecd54" {
		return "https://bldr.habitat.sh/v1/depot/pkgs/effortless/audit-baseline/0.1.0/20191217125442/download"
	}

	if id == "1f9e6a1e-3381-487f-98ef-9966c65ff92a" {
		return "https://github.com/dev-sec/apache-baseline/archive/master.tar.gz"
	}

	return "https://media.giphy.com/media/NkW4LM727h1U4/giphy.gif"
}
