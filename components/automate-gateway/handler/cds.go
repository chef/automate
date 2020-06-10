package handler

import (
	"context"

	log "github.com/sirupsen/logrus"

	"github.com/chef/automate/api/external/cds/request"
	"github.com/chef/automate/api/external/cds/response"
)

type CdsServer struct{}

func NewCdsServer() *CdsServer {
	return &CdsServer{}
}

// GetContentItems - Returns a list of CDS content metadata
func (s *CdsServer) GetContentItems(ctx context.Context, request *request.ContentItems) (*response.ContentItems, error) {
	log.WithFields(log.Fields{
		"request": request.String(),
		"func":    nameOfFunc(),
	}).Debug("rpc call")

	return &response.ContentItems{}, nil
}
