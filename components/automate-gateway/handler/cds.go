package handler

import (
	"context"

	log "github.com/sirupsen/logrus"

	"github.com/chef/automate/api/external/cds/request"
	"github.com/chef/automate/api/external/cds/response"
	inter_service "github.com/chef/automate/api/interservice/cds/service"
)

// CdsServer stores inter-service automate-cds client
type CdsServer struct {
	client inter_service.AutomateCdsClient
}

// NewCdsServer initializes CdsServer with client
func NewCdsServer(client inter_service.AutomateCdsClient) *CdsServer {
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
