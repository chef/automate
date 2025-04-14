package server

import (
	"context"

	"google.golang.org/grpc"
	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/status"

	api "github.com/chef/automate/api/interservice/deployment"
	ingest "github.com/chef/automate/api/interservice/ingest"
)

// GetReindexStatus forwards the request to the ingest-service
func (s *server) GetReindexStatus(ctx context.Context, req *api.GetReindexStatusRequest) (*api.GetReindexStatusResponse, error) {
	// Get address of ingest-service
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
	defer ingestConnection.Close() // Close connection after function execution

	// Create ingest-service client
	ingestClient := ingest.NewChefIngesterServiceClient(ingestConnection)

	// Forward request to ingest-service
	ingestReq := &ingest.GetReindexStatusRequest{
		RequestId: req.RequestId,
	}
	ingestResp, err := ingestClient.GetReindexStatus(ctx, ingestReq)
	if err != nil {
		return nil, status.Errorf(codes.Unavailable, "error fetching reindex status from ingest-service: %s", err.Error())
	}

	// Return the response from ingest-service
	return &api.GetReindexStatusResponse{
		StatusJson: ingestResp.StatusJson,
	}, nil
}

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

func (s *server) GetEligilbeIndexes(ctx context.Context, req *api.GetEligilbeIndexesRequest) (*api.GetEligilbeIndexesResponse, error) {
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

	ingestResp, err := ingestClient.GetEligilbeIndexes(ctx, &ingest.GetEligilbeIndexesRequest{})
	if err != nil {
		return nil, status.Errorf(codes.Unavailable, "error starting reindexing: %s", err.Error())
	}

	return &api.GetEligilbeIndexesResponse{
		Indexes: ingestResp.Indexes,
	}, nil
}
