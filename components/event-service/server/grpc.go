package server

import (
	"context"
	"net"

	"google.golang.org/grpc"
	"google.golang.org/grpc/reflection"

	es "github.com/chef/automate/api/interservice/event"
	"github.com/chef/automate/components/event-service/config"
	"github.com/chef/automate/lib/grpc/health"
	"github.com/chef/automate/lib/grpc/secureconn"
	"github.com/chef/automate/lib/tracing"
	"github.com/pkg/errors"
)

// NewGRPC creates the gRPC server.
func NewGRPC(ctx context.Context, config *config.EventConfig) (*grpc.Server, error) {
	// Set up our gRPC connection factory
	connFactory := secureconn.NewFactory(*config.ServiceCerts)

	srv, err := New(config)
	if err != nil {
		return nil, err
	}

	// Register our API
	grpcServer := connFactory.NewServer(tracing.GlobalServerInterceptor())
	es.RegisterEventServiceServer(grpcServer, srv)
	health.RegisterHealthServer(grpcServer, srv.health)

	// Register reflection service on gRPC server.
	reflection.Register(grpcServer)

	return grpcServer, nil
}

// StartGRPC starts the gRPC server
func StartGRPC(ctx context.Context, config *config.EventConfig) error {
	g, err := NewGRPC(ctx, config)
	if err != nil {
		return err
	}

	// Create our listener channel
	listener, err := net.Listen("tcp", config.ListenAddress())
	if err != nil {
		return errors.Wrapf(err, "failed to create tcp listener on address: %s", config.ListenAddress())
	}

	return g.Serve(listener)
}
