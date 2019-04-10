package server

import (
	"context"

	"google.golang.org/grpc"
	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/status"

	iReq "github.com/chef/automate/api/external/ingest/request"
	api "github.com/chef/automate/api/interservice/deployment"
	ingest "github.com/chef/automate/api/interservice/ingest"
)

func (s *server) InfrastructureNodeDelete(ctx context.Context,
	req *api.InfrastructureNodeDeleteRequest) (*api.InfrastructureNodeDeleteResponse, error) {

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

	defer ingestConnection.Close() // nolint: errcheck

	ingestClient := ingest.NewChefIngesterClient(ingestConnection)
	nodesRequest := &iReq.Delete{
		NodeId: req.NodeId,
	}

	_, err = ingestClient.ProcessNodeDelete(ctx, nodesRequest)
	if err != nil {
		return nil, status.Errorf(codes.Unavailable, "error deleting node: %s", err.Error())
	}

	return &api.InfrastructureNodeDeleteResponse{}, nil
}
