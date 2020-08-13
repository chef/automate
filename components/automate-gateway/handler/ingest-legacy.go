package handler

import (
	"context"

	gp "github.com/golang/protobuf/ptypes/empty"

	"github.com/chef/automate/api/interservice/ingest"
	"github.com/chef/automate/components/automate-gateway/api/legacy"
)

type legacyIngestServer struct {
	ingestStatusServiceClient ingest.IngestStatusServiceClient
}

func NewLegacyIngestServer(ingestStatusServiceClient ingest.IngestStatusServiceClient) *legacyIngestServer {
	return &legacyIngestServer{ingestStatusServiceClient: ingestStatusServiceClient}
}

func (s *legacyIngestServer) Status(ctx context.Context, _ *gp.Empty) (*legacy.StatusResponse, error) {
	resp, err := s.ingestStatusServiceClient.GetHealth(ctx, &ingest.HealthRequest{})
	if err != nil {
		return nil, err
	}
	return &legacy.StatusResponse{Status: resp.Status}, nil
}
