package handler

import (
	"context"

	log "github.com/sirupsen/logrus"

	external_service "github.com/chef/automate/api/external/cds"
	"github.com/chef/automate/api/external/cds/request"
	"github.com/chef/automate/api/external/cds/response"
	inter_service "github.com/chef/automate/api/interservice/cds/service"
)

// CdsServer stores inter-service automate-cds client
type CdsServer struct {
	client inter_service.AutomateCdsServiceClient
}

// NewCdsServer initializes CdsServer with client
func NewCdsServer(client inter_service.AutomateCdsServiceClient) *CdsServer {
	return &CdsServer{client}
}

// ListContentItems - Returns a list of CDS content metadata
func (s *CdsServer) ListContentItems(ctx context.Context,
	request *request.ContentItems) (*response.ContentItems, error) {
	log.WithFields(log.Fields{
		"request": request.String(),
		"func":    nameOfFunc(),
	}).Debug("rpc call")

	return s.client.ListContentItems(ctx, request)
}

// InstallContentItem - Returns a list of CDS content metadata
func (s *CdsServer) InstallContentItem(ctx context.Context,
	request *request.InstallContentItem) (*response.InstallContentItem, error) {
	log.WithFields(log.Fields{
		"request": request.String(),
		"func":    nameOfFunc(),
	}).Debug("rpc call")

	return s.client.InstallContentItem(ctx, request)
}

func (a *CdsServer) DownloadContentItem(*request.DownloadContentItem, external_service.Cds_DownloadContentItemServer) error {
	// grpc gateway is not able to handle streaming; https://github.com/grpc-ecosystem/grpc-gateway/issues/435
	// so we do not auto-generate the route for download; we instead custom handle with mux in gateway/services.go
	return nil
}

// SubmitToken - submit a token to enable content
func (s *CdsServer) SubmitCredentials(ctx context.Context,
	request *request.Credentials) (*response.Credentials, error) {
	log.WithFields(log.Fields{
		"request": request.String(),
		"func":    nameOfFunc(),
	}).Debug("rpc call")

	return s.client.SubmitCredentials(ctx, request)
}

// IsContentEnabled - check if content is enabled
func (s *CdsServer) IsContentEnabled(ctx context.Context,
	request *request.ContentEnabled) (*response.ContentEnabled, error) {
	log.WithFields(log.Fields{
		"request": request.String(),
		"func":    nameOfFunc(),
	}).Debug("rpc call")

	return s.client.IsContentEnabled(ctx, request)
}
