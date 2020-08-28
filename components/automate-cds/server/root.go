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

	"github.com/chef/automate/components/automate-cds/cloud"
	"github.com/chef/automate/components/automate-cds/creds"
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
	cloud                 cloud.Client
	secrets               creds.Secrets
}

// NewServer returns an Automate CDS proxy server
func NewServer(service *service.Service,
	profilesServiceClient profiles.ProfilesServiceClient) *Server {
	return &Server{
		service:               service,
		profilesServiceClient: profilesServiceClient,
		cloud:                 cloud.NewClient(),
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

	credentials, found, err := s.secrets.GetCredentials()
	if err != nil {
		return &response.ContentItems{}, status.Errorf(codes.Internal,
			"Could not retrieve credentials from secrets-service error: %s", err.Error())
	}

	if !found {
		return &response.ContentItems{}, status.Errorf(codes.InvalidArgument,
			"Can not request content items without a credentials submitted")
	}

	cloudItems, err := s.cloud.GetContentItems(credentials)
	if err != nil {
		return &response.ContentItems{}, status.Errorf(codes.Internal,
			"Failed connecting to Chef Cloud")
	}

	responseItems := make([]*response.ContentItem, len(cloudItems))
	for index, cloudItem := range cloudItems {
		responseItems[index] = &response.ContentItem{
			Id:             cloudItem.ID,
			Name:           cloudItem.Name,
			Description:    cloudItem.Description,
			Type:           cloudItem.Type,
			Version:        cloudItem.Version,
			Platforms:      cloudItem.Platforms,
			CanBeInstalled: cloudItem.CanBeInstalled,
			Filename:       cloudItem.Filename,
		}
	}

	return &response.ContentItems{
		Items: responseItems,
	}, nil
}

// IsContentEnabled - Check if the content is enable for this Automate instance.
func (s *Server) IsContentEnabled(ctx context.Context,
	request *request.ContentEnabled) (*response.ContentEnabled, error) {

	credentials, found, err := s.secrets.GetCredentials()
	if err != nil {
		return &response.ContentEnabled{}, status.Errorf(codes.Internal,
			"Could not retrieve credentials from secrets-service error: %s", err.Error())
	}

	if !found {
		return &response.ContentEnabled{
			IsContentEnabled: false,
		}, nil
	}

	ok, err := s.cloud.VerifyCredentials(credentials)
	if err != nil {
		return &response.ContentEnabled{}, status.Errorf(codes.Internal, "Could not verify the credentials with Chef Cloud. error: %s", err.Error())
	}
	if !ok {
		return &response.ContentEnabled{}, status.Errorf(codes.InvalidArgument, "Chef Cloud does not recognize the provided credentials")
	}

	return &response.ContentEnabled{
		IsContentEnabled: true,
	}, nil
}

// SubmitCredentials - Submit a Chef Cloud credentials to enable content
func (s *Server) SubmitCredentials(ctx context.Context,
	request *request.Credentials) (*response.Credentials, error) {

	if len(request.ClientId) == 0 ||
		len(request.ClientSecret) == 0 ||
		len(request.TenantSpecificUrl) == 0 {
		return &response.Credentials{}, status.Errorf(codes.InvalidArgument,
			"the client ID, client secret, and tenant specific URL must not be empty")
	}

	credentials := creds.CreateCredentials(request.ClientId, request.ClientSecret, request.TenantSpecificUrl)

	ok, err := s.cloud.VerifyCredentials(credentials)
	if err != nil {
		return &response.Credentials{}, status.Errorf(codes.Internal, "Could not verify the credentials with Chef Cloud. error: %s", err.Error())
	}
	if !ok {
		return &response.Credentials{}, status.Errorf(codes.InvalidArgument, "Chef Cloud does not recognize the provided credentials")
	}

	s.secrets.AddCredentials(credentials)

	return &response.Credentials{}, nil
}

// InstallContentItem - installing a content item
func (s *Server) InstallContentItem(ctx context.Context,
	request *request.InstallContentItem) (*response.InstallContentItem, error) {
	credentials, found, err := s.secrets.GetCredentials()
	if err != nil {
		return &response.InstallContentItem{}, status.Errorf(codes.Internal,
			"Could not retrieve credentials from secrets-service error: %s", err.Error())
	}

	if !found {
		return &response.InstallContentItem{}, status.Errorf(codes.InvalidArgument,
			"Can not install a content item without a credentials submitted")
	}
	contentItem, found, err := s.cloud.GetContentItem(request.Id, credentials)
	if err != nil {
		return &response.InstallContentItem{}, status.Errorf(codes.Internal,
			"Could not connect to Content Delivery Service error: %s", err.Error())
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

		err = s.installProfile(ctx, contentItem, request.RequestUser, credentials)
		if err != nil {
			return &response.InstallContentItem{}, status.Errorf(codes.Internal,
				"Error Installing Profile: %s", err.Error())
		}
	} else {
		return &response.InstallContentItem{}, status.Errorf(codes.InvalidArgument,
			"Can install content item type %q", contentItem.Type)
	}

	return &response.InstallContentItem{}, nil
}

// DownloadContentItem - download content item
func (s *Server) DownloadContentItem(request *request.DownloadContentItem,
	stream interservice.AutomateCdsService_DownloadContentItemServer) error {

	credentials, found, err := s.secrets.GetCredentials()
	if err != nil {
		return status.Errorf(codes.Internal,
			"Could not retrieve credentials from secrets-service error: %s", err.Error())
	}

	if !found {
		return status.Errorf(codes.InvalidArgument,
			"Can not download a content item without a credentials submitted")
	}

	contentItem, found, err := s.cloud.GetContentItem(request.Id, credentials)
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
	contentItem cloud.ContentItem, owner string, credentials creds.Credentials) error {
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
