package server

import (
	"context"
	"net"

	"github.com/chef/automate/components/license-control-service/licenseaudit"
	"github.com/chef/automate/lib/cereal/postgres"
	"github.com/gofrs/uuid"
	"github.com/pkg/errors"
	log "github.com/sirupsen/logrus"
	"google.golang.org/grpc"
	"google.golang.org/grpc/reflection"

	lc "github.com/chef/automate/api/interservice/license_control"
	"github.com/chef/automate/components/license-control-service/pkg/keys"
	"github.com/chef/automate/components/license-control-service/pkg/storage"
	"github.com/chef/automate/lib/cereal"
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

	// Setup our gRPC connection factory
	connFactory := secureconn.NewFactory(*config.ServiceCerts)
	cerealManager, err := startCerealService(ctx, connFactory, config)
	if err != nil {
		log.Errorf("unable to execute and register cereal service for license control service %v", err.Error())
	}

	err = cerealManager.Start(context.Background())
	if err != nil {
		log.Errorf("Unable to start the cereal Manager for license control service %v", err)
	}

	defer cerealManager.Stop()

	return g.Serve(listener)
}

// storeDeploymentID: Stores the deployment id in DB
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

func startCerealService(timeoutCtx context.Context, connFactory *secureconn.Factory, config *Config) (*cereal.Manager, error) {
	// cerealConn, err := connFactory.Dial("cereal-service", config.CerealHost)
	// if err != nil {
	// 	return errors.Wrap(err, "error dialing cereal service")
	//}

	//cerealBackend := grpccereal.NewGrpcBackendFromConn("license-control-service", cerealConn)

	// Set up a cereal (workflow) manager for managing nodemanager workflows.
	cerealManager, err := cereal.NewManager(postgres.NewPostgresBackend(config.PGURL))
	if err != nil {
		return nil, errors.Wrap(err, "could not create cereal manager")
	}

	//defer cerealManager.Stop() //nolint:errcheck

	err = licenseaudit.InitCerealManager(context.TODO(), cerealManager, 1)
	if err != nil {
		return nil, errors.Wrap(err, "could not create cereal manager")
	}
	// err = patterns.RegisterSingleTaskWorkflowExecutor(
	// 	cerealManager,
	// 	AuditWorkflowName,
	// 	true,
	// 	&LicenseAuditTask{},
	// 	cereal.TaskExecutorOpts{Workers: 1, Timeout: 12 * time.Second})
	// if err != nil {
	// 	return errors.Wrapf(err, "failed to register license audit workflow")
	// }

	//Create recurrence role.

	return cerealManager, nil
}
