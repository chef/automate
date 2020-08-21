package server

import (
	"context"
	"net"

	grpc_middleware "github.com/grpc-ecosystem/go-grpc-middleware"
	grpc_logrus "github.com/grpc-ecosystem/go-grpc-middleware/logging/logrus"
	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"
	"google.golang.org/grpc"
	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/grpclog"
	"google.golang.org/grpc/reflection"
	"google.golang.org/grpc/status"

	project_update_tags "github.com/chef/automate/lib/authz"
	"github.com/chef/automate/lib/cereal"
	"github.com/chef/automate/lib/grpc/health"
	"github.com/chef/automate/lib/grpc/secureconn"
	"github.com/chef/automate/lib/logger"
	"github.com/chef/automate/lib/tracing"

	api "github.com/chef/automate/api/interservice/authz"
	"github.com/chef/automate/components/authz-service/engine"
	"github.com/chef/automate/components/authz-service/storage/postgres"
	"github.com/chef/automate/components/authz-service/storage/postgres/datamigration"
	"github.com/chef/automate/components/authz-service/storage/postgres/migration"
)

// GRPC creates and listens on grpc server.
func GRPC(ctx context.Context,
	addr string, l logger.Logger, connFactory *secureconn.Factory,
	e engine.Engine, migrationsConfig migration.Config,
	dataMigrationsConfig datamigration.Config, cerealAddress string,
	projectLimit int) error {

	grpclog.SetLoggerV2(l)
	list, err := net.Listen("tcp", addr)
	if err != nil {
		return err
	}
	l.Printf("Authz GRPC API listening on %s", addr)

	serv, err := NewGRPCServer(ctx, connFactory, l, e, migrationsConfig,
		dataMigrationsConfig, cerealAddress, projectLimit)
	if err != nil {
		return err
	}

	return serv.Serve(list)
}

// NewGRPCServer creates a grpc server.
func NewGRPCServer(ctx context.Context,
	connFactory *secureconn.Factory, l logger.Logger,
	e engine.Engine, migrationsConfig migration.Config,
	dataMigrationsConfig datamigration.Config, cerealAddress string,
	projectLimit int) (*grpc.Server, error) {

	err := postgres.Initialize(ctx, e, l, migrationsConfig, dataMigrationsConfig, projectLimit)
	if err != nil {
		return nil, errors.Wrap(err, "could not initialize postgres singleton")
	}

	cerealManager, err := createProjectUpdateCerealManager(connFactory, cerealAddress)
	if err != nil {
		return nil, errors.Wrap(err, "could not create cereal manager")
	}

	policyRefresher, err := NewPostgresPolicyRefresher(ctx, l, e)
	if err != nil {
		return nil, errors.Wrap(err, "could not initialize policy refresher")
	}

	projectsServer, err := NewPostgresProjectsServer(ctx, l, cerealManager, policyRefresher)
	if err != nil {
		return nil, errors.Wrap(err, "could not initialize projects server")
	}

	authzServer, err := NewPostgresAuthzServer(l, e, projectsServer)
	if err != nil {
		return nil, errors.Wrap(err, "could not initialize authz server")
	}

	polServer, err := NewPostgresPolicyServer(ctx, l, policyRefresher, e)
	if err != nil {
		return nil, errors.Wrap(err, "could not initialize policy server")
	}

	// This function determines the log level based on the returned status code:
	// We divert from the default only by pushing non-error-returns into debug
	// logs. See DefaultCodeToLevel for the rest of the mapping.
	levelFunc := func(c codes.Code) logrus.Level {
		switch c {
		case codes.OK:
			return logrus.DebugLevel
		default:
			return grpc_logrus.DefaultCodeToLevel(c)
		}
	}

	logrusEntry := l.NewEntry()

	logrusOpts := []grpc_logrus.Option{
		grpc_logrus.WithDecider(func(m string, _ error) bool {
			return m != "/grpc.health.v1.Health/Check"
		}),
		grpc_logrus.WithLevels(levelFunc),
	}

	g := connFactory.NewServer(
		grpc.UnaryInterceptor(
			grpc_middleware.ChainUnaryServer(
				tracing.ServerInterceptor(tracing.GlobalTracer()),
				grpc_logrus.UnaryServerInterceptor(logrusEntry, logrusOpts...),
				InputValidationInterceptor(),
				polServer.EngineUpdateInterceptor(),
			),
		),
	)

	// register all services
	health.RegisterHealthServer(g, health.NewService())
	api.RegisterPoliciesServiceServer(g, polServer)
	api.RegisterProjectsServiceServer(g, projectsServer)
	api.RegisterAuthorizationServiceServer(g, authzServer)
	reflection.Register(g)

	if err := cerealManager.Start(ctx); err != nil {
		return nil, errors.Wrap(err, "failed to start cereal manager")
	}
	return g, nil
}

func createProjectUpdateCerealManager(connFactory *secureconn.Factory, address string) (*cereal.Manager, error) {
	conn, err := connFactory.Dial("cereal-service", address)
	if err != nil {
		return nil, errors.Wrap(err, "error dialing cereal service")
	}

	grpcBackend := project_update_tags.ProjectUpdateBackend(conn)
	manager, err := cereal.NewManager(grpcBackend)
	if err != nil {
		grpcBackend.Close() // nolint: errcheck
		return nil, err
	}

	return manager, nil
}

// InputValidationInterceptor is a middleware for running the protobuf validation.
func InputValidationInterceptor() grpc.UnaryServerInterceptor {
	return func(ctx context.Context,
		req interface{},
		_ *grpc.UnaryServerInfo,
		handler grpc.UnaryHandler) (interface{}, error) {
		if req, ok := req.(interface {
			Validate() error
		}); ok {
			if err := req.Validate(); err != nil {
				return nil, status.Error(codes.InvalidArgument, err.Error())
			}
		}
		return handler(ctx, req)
	}
}
