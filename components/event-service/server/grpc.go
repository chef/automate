package server

import (
	"context"
	"net"

	"fmt"

	log "github.com/sirupsen/logrus"
	"google.golang.org/grpc"
	"google.golang.org/grpc/reflection"

	es "github.com/chef/automate/api/interservice/event"
	"github.com/chef/automate/components/event-service/config"
	"github.com/chef/automate/lib/grpc/health"
	"github.com/chef/automate/lib/grpc/secureconn"
	"github.com/chef/automate/lib/tracing"
)

// NewGRPC creates the gRPC server.
func NewGRPC(ctx context.Context, config *config.EventConfig) *grpc.Server {
	// Set up our gRPC connection factory
	connFactory := secureconn.NewFactory(*config.ServiceCerts)

	// Register our API
	grpcServer := connFactory.NewServer(tracing.GlobalServerInterceptor())
	srv := New(config)
	es.RegisterEventServiceServer(grpcServer, srv)
	health.RegisterHealthServer(grpcServer, srv.health)

	// Register reflection service on gRPC server.
	reflection.Register(grpcServer)
	return grpcServer
}

// StartGRPC starts the gRPC server
func StartGRPC(ctx context.Context, config *config.EventConfig) *grpc.Server {
	g := NewGRPC(ctx, config)

	// Create our listener channel
	listener, err := net.Listen("tcp", config.ListenAddress())
	if err != nil {
		log.WithFields(log.Fields{
			"err": err,
		}).Fatal(fmt.Sprintf("could not listen on address: %s", config.ListenAddress()))
	}

	g.Serve(listener)
	return g
}
