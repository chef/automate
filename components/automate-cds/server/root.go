package server

import (
	"bufio"
	"context"
	"fmt"
	"io/ioutil"

	"github.com/chef/automate/api/external/cds/request"
	"github.com/chef/automate/api/external/cds/response"
	"github.com/chef/automate/api/external/common"
	ver_api "github.com/chef/automate/api/external/common/version"
	interservice "github.com/chef/automate/api/interservice/cds/service"

	"github.com/chef/automate/components/automate-cds/backend"
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
	client                backend.Client
}

// NewServer returns an Automate CDS proxy server
func NewServer(service *service.Service,
	profilesServiceClient profiles.ProfilesServiceClient) *Server {
	return &Server{
		service:               service,
		profilesServiceClient: profilesServiceClient,
		client:                backend.NewClient(),
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
func (s *Server) ListContentItems(ctx context.Context,
	request *request.ContentItems) (*response.ContentItems, error) {

	backendItems, err := s.client.GetContentItems()
	if err != nil {
		return &response.ContentItems{}, status.Errorf(codes.Internal,
			"Failed connecting to Chef Cloud")
	}

	responseItems := make([]*response.ContentItem, len(backendItems))
	for index, backendItem := range backendItems {
		responseItems[index] = &response.ContentItem{
			Id:             backendItem.ID,
			Name:           backendItem.Name,
			Description:    backendItem.Description,
			Type:           backendItem.Type,
			Version:        backendItem.Version,
			Platforms:      backendItem.Platforms,
			CanBeInstalled: backendItem.CanBeInstalled,
			Filename:       backendItem.Filename,
		}
	}

	return &response.ContentItems{
		Items: responseItems,
	}, nil
}

// InstallContentItem - installing a content item
func (s *Server) InstallContentItem(ctx context.Context,
	request *request.InstallContentItem) (*response.InstallContentItem, error) {
	contentItem, found, err := s.client.GetContentItem(request.Id)
	if err != nil {
		return &response.InstallContentItem{}, status.Errorf(codes.Internal,
			"Could not connect to Content Delivery Service error: %s", err.Error)
	}
	if !found {
		return &response.InstallContentItem{}, status.Errorf(codes.InvalidArgument,
			"Content item not found with ID %q", request.Id)
	}

	if !contentItem.CanBeInstalled {
		return &response.InstallContentItem{}, status.Errorf(codes.InvalidArgument,
			"Content item with ID %q Can not be installed", request.Id)
	}

	if contentItem.Type == "profile" {
		if request.RequestUser == "" {
			return &response.InstallContentItem{}, status.Errorf(codes.InvalidArgument,
				"A request_user must be non empty")
		}

		err = s.installProfile(ctx, contentItem, request.RequestUser)
		if err != nil {
			return &response.InstallContentItem{}, status.Errorf(codes.Internal,
				"Error Installing Profile: %s", err.Error)
		}
	} else {
		return &response.InstallContentItem{}, status.Errorf(codes.InvalidArgument,
			"Can install content item type %q", contentItem.Type)
	}

	return &response.InstallContentItem{}, nil
}

// DownloadContentItem - download content item
func (s *Server) DownloadContentItem(request *request.DownloadContentItem,
	stream interservice.AutomateCds_DownloadContentItemServer) error {
	contentItem, found, err := s.client.GetContentItem(request.Id)
	if err != nil {
		return err
	}

	if !found {
		return fmt.Errorf("Content item not found with ID %q", request.Id)
	}

	log.Infof("DownloadContentItem: Downloading content item with ID %s ...", request.Id)

	// Get the data
	resp, err := http.Get(contentItem.DownloadURL)
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

func (s *Server) installProfile(ctx context.Context,
	contentItem backend.ContentItem, owner string) error {
	log.Infof("Installing profile content item with ID %s ...", contentItem.ID)

	// Get the data
	resp, err := http.Get(contentItem.DownloadURL)
	if err != nil {
		return status.Errorf(codes.Internal, "Could not connect to Content Delivery Service")
	}
	defer resp.Body.Close()

	stream, err := s.profilesServiceClient.Create(ctx)
	if err != nil {
		return status.Errorf(codes.Internal, "Failed connecting to the compliance-service")
	}

	body, err := ioutil.ReadAll(resp.Body)
	if err != nil {
		return status.Errorf(codes.Internal, "Failed streaming from SaaS")
	}

	err = stream.Send(&profiles.ProfilePostRequest{
		Owner: owner,
		Chunk: &profiles.Chunk{Data: body},
		Meta: &profiles.Metadata{
			ContentType: "application/gzip",
		},
	})
	if err != nil {
		return status.Errorf(codes.Internal, "Failed sending to the compliance service")
	}

	_, err = stream.CloseAndRecv()
	if err != nil {
		return status.Errorf(codes.Internal,
			"Failed closing stream to the compliance service error: %q", err.Error())
	}

	return nil
}
