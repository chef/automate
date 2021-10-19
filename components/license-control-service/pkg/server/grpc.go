package server

import (
	"context"
	"net"

	"github.com/gofrs/uuid"
	"github.com/pkg/errors"
	"google.golang.org/grpc"
	"google.golang.org/grpc/reflection"

	lc "github.com/chef/automate/api/interservice/license_control"
	"github.com/chef/automate/components/license-control-service/pkg/keys"
	"github.com/chef/automate/components/license-control-service/pkg/storage"
	"github.com/chef/automate/lib/grpc/health"
	"github.com/chef/automate/lib/grpc/secureconn"
	"github.com/chef/automate/lib/tracing"
)

// NewGRPC creates the gRPC server.
func NewGRPC(ctx context.Context, config *Config) (*grpc.Server, error) {
	// Setup our gRPC connection factory
	connFactory := secureconn.NewFactory(*config.ServiceCerts)

	// Register our API
	grpcServer := connFactory.NewServer(tracing.GlobalServerInterceptor())

	licenseParser := keys.NewLicenseParser(keys.BuiltinKeyData)
	backend := storage.NewCurrentBackend(config.PGURL, config.MigrationsPath, config.LicenseTokenPath)
	err := backend.Init(ctx, licenseParser)
	if err != nil {
		return nil, errors.Wrap(err, "failed to initialize storage backend")
	}
	// Create deployment id and save it in DB
	err = storeDeploymentID(backend)
	if err != nil {
		return nil, err
	}
	srv := NewLicenseControlServer(ctx, backend, licenseParser, config)
	lc.RegisterLicenseControlServiceServer(grpcServer, srv)
	health.RegisterHealthServer(grpcServer, srv.health)

	// Register reflection service on gRPC server.
	reflection.Register(grpcServer)
	return grpcServer, nil
}

// StartGRPC starts the gRPC server
func StartGRPC(ctx context.Context, config *Config) error {
	g, err := NewGRPC(ctx, config)
	if err != nil {
		return errors.Wrap(err, "could not initialize server")
	}

	// Create our listener channel
	listener, err := net.Listen("tcp", config.ListenAddress())
	if err != nil {
		return errors.Wrapf(err, "could no listen on %s", config.ListenAddress())
	}

	return g.Serve(listener)
}

//storeDeploymentID: Stores the deployment id in DB
func storeDeploymentID(backend storage.CurrentBackend) error {
	id, err := uuid.NewV4()
	if err != nil {
		return errors.Wrap(err, "failed to create deployment id")
	}
	deploymentId := id.String()
	err = backend.StoreDeployment(context.Background(), deploymentId)
	if err != nil {
		return errors.Wrap(err, "failed to save deployment id")
	}
	return nil
}
