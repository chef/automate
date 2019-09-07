package server

import (
	"context"
	"net"

	"github.com/pkg/errors"
	"google.golang.org/grpc"
	"google.golang.org/grpc/reflection"

	"github.com/chef/automate/components/data-feed-service/config"
	"github.com/chef/automate/lib/grpc/health"
	"github.com/chef/automate/lib/grpc/secureconn"
	"github.com/chef/automate/lib/tracing"
)

type Server struct {
	cfg    *config.DataFeedConfig
	health *health.Service
}

// NewGRPC creates the gRPC server.
func NewGRPC(ctx context.Context, config *config.DataFeedConfig, connFactory *secureconn.Factory) *grpc.Server {
	grpcServer := connFactory.NewServer(tracing.GlobalServerInterceptor())
	srv := &Server{
		cfg:    config,
		health: health.NewService(),
	}
	health.RegisterHealthServer(grpcServer, srv.health)

	// Register reflection service on gRPC server.
	reflection.Register(grpcServer)
	return grpcServer
}

// StartGRPC starts the gRPC server
func StartGRPC(ctx context.Context, config *config.DataFeedConfig, connFactory *secureconn.Factory) error {
	g := NewGRPC(ctx, config, connFactory)

	// Create our listener channel
	listener, err := net.Listen("tcp", config.ListenAddress())
	if err != nil {
		return errors.Wrapf(err, "could not listen on address: %s", config.ListenAddress())
	}

	return g.Serve(listener)
}
