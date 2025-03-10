package server

import (
	"context"

	"google.golang.org/grpc"
	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/status"

	api "github.com/chef/automate/api/interservice/deployment"
	ingest "github.com/chef/automate/api/interservice/ingest"
)

func (s *server) StartReindex(ctx context.Context, req *api.StartReindexRequest) (*api.StartReindexResponse, error) {
	ingestAddr := s.AddressForService("ingest-service")
	ingestConnection, err := s.connFactory.DialContext(
		ctx,
		"ingest-service",
		ingestAddr,
		grpc.WithBlock(),
	)
	if err != nil {
		return nil, status.Errorf(codes.Unavailable, "error connecting to ingest-service: %s", err.Error())
	}
	defer ingestConnection.Close()

	ingestClient := ingest.NewChefIngesterServiceClient(ingestConnection)

	ingestResp, err := ingestClient.StartReindex(ctx, &ingest.StartReindexRequest{})
	if err != nil {
		return nil, status.Errorf(codes.Unavailable, "error starting reindexing: %s", err.Error())
	}

	return &api.StartReindexResponse{
		Message: ingestResp.Message,
	}, nil
}
