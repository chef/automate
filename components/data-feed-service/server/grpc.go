package server

import (
	"context"
	"net"

	datafeed "github.com/chef/automate/api/external/data_feed"
	"github.com/pkg/errors"
	"google.golang.org/grpc"
	"google.golang.org/grpc/reflection"

	"github.com/chef/automate/components/data-feed-service/config"
	"github.com/chef/automate/components/data-feed-service/dao"
	"github.com/chef/automate/lib/grpc/health"
	"github.com/chef/automate/lib/grpc/secureconn"
	"github.com/chef/automate/lib/tracing"
)

// NewGRPC creates the gRPC server.
func NewGRPC(ctx context.Context, config *config.DataFeedConfig, connFactory *secureconn.Factory, db *dao.DB) (*grpc.Server, error) {
	grpcServer := connFactory.NewServer(tracing.GlobalServerInterceptor())

	datafeedServer, err := NewDatafeedServer(db, config, connFactory)
	if err != nil {
		return nil, err
	}
	datafeed.RegisterDatafeedServiceServer(grpcServer, datafeedServer)
	health.RegisterHealthServer(grpcServer, datafeedServer.health)

	// Register reflection service on gRPC server.
	reflection.Register(grpcServer)

	return grpcServer, nil
}

// StartGRPC starts the gRPC server
func StartGRPC(ctx context.Context, config *config.DataFeedConfig, connFactory *secureconn.Factory, db *dao.DB) error {
	g, err := NewGRPC(ctx, config, connFactory, db)
	if err != nil {
		return err
	}
	// Create our listener channel
	listener, err := net.Listen("tcp", config.ListenAddress())
	if err != nil {
		return errors.Wrapf(err, "could not listen on address: %s", config.ListenAddress())
	}

	return g.Serve(listener)
}
