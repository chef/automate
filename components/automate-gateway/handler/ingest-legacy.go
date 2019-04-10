package handler

import (
	"context"

	gp "github.com/golang/protobuf/ptypes/empty"

	"github.com/chef/automate/api/interservice/ingest"
	"github.com/chef/automate/components/automate-gateway/api/legacy"
)

type legacyIngestServer struct {
	ingestStatusClient ingest.IngestStatusClient
}

func NewLegacyIngestServer(ingestStatusClient ingest.IngestStatusClient) *legacyIngestServer {
	return &legacyIngestServer{ingestStatusClient: ingestStatusClient}
}

func (s *legacyIngestServer) Status(ctx context.Context, _ *gp.Empty) (*legacy.StatusResponse, error) {
	resp, err := s.ingestStatusClient.GetHealth(ctx, &ingest.HealthRequest{})
	if err != nil {
		return nil, err
	}
	return &legacy.StatusResponse{Status: resp.Status}, nil
}
