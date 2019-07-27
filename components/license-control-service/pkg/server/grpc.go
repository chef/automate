package server

import (
	"context"
	"fmt"
	"net"

	"github.com/pkg/errors"
	log "github.com/sirupsen/logrus"
	"google.golang.org/grpc"
	"google.golang.org/grpc/reflection"

	lc "github.com/chef/automate/api/interservice/license_control"
	"github.com/chef/automate/lib/grpc/health"
	"github.com/chef/automate/lib/grpc/secureconn"
	"github.com/chef/automate/lib/tracing"
)

// NewGRPC creates the gRPC server.
func NewGRPC(ctx context.Context, config *Config) *grpc.Server {
	// Setup our gRPC connection factory
	connFactory := secureconn.NewFactory(*config.ServiceCerts)

	// Register our API
	grpcServer := connFactory.NewServer(tracing.GlobalServerInterceptor())
	srv := NewLicenseControlServer(ctx, config)
	lc.RegisterLicenseControlServer(grpcServer, srv)
	health.RegisterHealthServer(grpcServer, srv.health)

	// Register reflection service on gRPC server.
	reflection.Register(grpcServer)
	return grpcServer
}

// StartGRPC starts the gRPC server
func StartGRPC(ctx context.Context, config *Config) error {
	g := NewGRPC(ctx, config)

	// Create our listener channel
	listener, err := net.Listen("tcp", config.ListenAddress())
	if err != nil {
		log.WithFields(log.Fields{
			"err": err,
		}).Fatal(fmt.Sprintf("could not listen on address: %s", config.ListenAddress()))
		return errors.Wrap(err, "listen")
	}

	return g.Serve(listener)
}
