package server

import (
	"context"
	"net"
	"strings"

	automate_event "github.com/chef/automate/api/interservice/event"
	grpc_middleware "github.com/grpc-ecosystem/go-grpc-middleware"
	grpc_logrus "github.com/grpc-ecosystem/go-grpc-middleware/logging/logrus"
	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"
	"google.golang.org/grpc"
	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/grpclog"
	"google.golang.org/grpc/reflection"
	"google.golang.org/grpc/status"

	"github.com/chef/automate/lib/grpc/health"
	"github.com/chef/automate/lib/grpc/secureconn"
	"github.com/chef/automate/lib/logger"
	"github.com/chef/automate/lib/tracing"

	"github.com/chef/automate/api/interservice/authz"
	"github.com/chef/automate/api/interservice/authz/common"
	api_v2 "github.com/chef/automate/api/interservice/authz/v2"
	"github.com/chef/automate/components/authz-service/engine"
	v1 "github.com/chef/automate/components/authz-service/server/v1"
	v2 "github.com/chef/automate/components/authz-service/server/v2"
	"github.com/chef/automate/components/authz-service/storage/postgres/datamigration"
	"github.com/chef/automate/components/authz-service/storage/postgres/migration"
)

// GRPC creates and listens on grpc server.
func GRPC(ctx context.Context,
	addr string, l logger.Logger, connFactory *secureconn.Factory,
	e engine.Engine, migrationsConfig migration.Config,
	dataMigrationsConfig datamigration.Config, eventServiceAddress string) error {

	grpclog.SetLoggerV2(l)
	list, err := net.Listen("tcp", addr)
	if err != nil {
		return err
	}
	l.Printf("Authz GRPC API listening on %s", addr)

	server, err := NewGRPCServer(ctx, connFactory, l, e, migrationsConfig,
		dataMigrationsConfig, eventServiceAddress)
	if err != nil {
		return err
	}

	return server.Serve(list)
}

// NewGRPCServer creates a grpc server.
func NewGRPCServer(ctx context.Context,
	connFactory *secureconn.Factory, l logger.Logger,
	e engine.Engine, migrationsConfig migration.Config,
	dataMigrationsConfig datamigration.Config,
	eventServiceAddress string) (*grpc.Server, error) {

	// Note(sr): we're buffering one version struct, as NewPostgresPolicyServer writes
	// to this before we've got readers
	vChan := make(chan api_v2.Version, 1)
	switcher := NewSwitch(vChan)

	v1Server, err := v1.NewPostgresServer(ctx, l, e, migrationsConfig)
	if err != nil {
		return nil, errors.Wrap(err, "could not initialize v1 server")
	}

	v2PolServer, err := v2.NewPostgresPolicyServer(ctx, l, e, migrationsConfig,
		dataMigrationsConfig, v1Server.Storage(), vChan)
	if err != nil {
		return nil, errors.Wrap(err, "could not initialize v2 policy server")
	}

	eventServiceClient, err := createEventServiceConnection(connFactory, eventServiceAddress)
	if err != nil {
		return nil, errors.Wrap(err, "could not create event service client")
	}

	v2ProjectsServer, err := v2.NewPostgresProjectsServer(ctx, l, migrationsConfig,
		dataMigrationsConfig, e, eventServiceClient)
	if err != nil {
		return nil, errors.Wrap(err, "could not initialize v2 projects server")
	}

	v2AuthzServer, err := v2.NewAuthzServer(l, e, v2PolServer)
	if err != nil {
		return nil, errors.Wrap(err, "could not initialize v2 authz server")
	}

	subjectPurgeServer, err := v2.NewSubjectPurgeServer(ctx, l, v1Server, v2PolServer)
	if err != nil {
		return nil, errors.Wrap(err, "could not initialize subject purge server")
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
				switcher.Interceptor,
				InputValidationInterceptor(),
				v2PolServer.EngineUpdateInterceptor(),
			),
		),
	)

	// register all services
	health.RegisterHealthServer(g, health.NewService())
	authz.RegisterAuthorizationServer(g, v1Server)
	api_v2.RegisterPoliciesServer(g, v2PolServer)
	api_v2.RegisterProjectsServer(g, v2ProjectsServer)
	api_v2.RegisterAuthorizationServer(g, v2AuthzServer)
	common.RegisterSubjectPurgeServer(g, subjectPurgeServer)
	reflection.Register(g)
	return g, nil
}

type versionSwitch struct {
	version api_v2.Version
}

func NewSwitch(c chan api_v2.Version) *versionSwitch {
	x := versionSwitch{
		version: api_v2.Version{
			Major: api_v2.Version_V1,
			Minor: api_v2.Version_V0,
		},
	}
	go func() {
		for {
			select {
			case v := <-c:
				x.version = v
			}
		}
	}()
	return &x
}

func createEventServiceConnection(connFactory *secureconn.Factory,
	eventServiceAddress string) (automate_event.EventServiceClient, error) {
	if eventServiceAddress == "" {
		return nil, errors.New("eventServiceAddress cannot be empty or Dial will get stuck")
	}

	conn, err := connFactory.Dial("event-service", eventServiceAddress)
	if err != nil {
		return nil, errors.Wrap(err, "Could not obtain EventServiceClient; error dialing event service")
	}

	eventServiceClient := automate_event.NewEventServiceClient(conn)
	if eventServiceClient == nil {
		return nil, errors.New("could not obtain NewEventServiceClient")
	}

	return eventServiceClient, nil
}

func (v *versionSwitch) Interceptor(ctx context.Context,
	req interface{},
	info *grpc.UnaryServerInfo,
	handler grpc.UnaryHandler) (interface{}, error) {
	// For Authorization calls (IsAuthorized, FilterAuthorizedPairs), we decline
	// cross-overs.
	//
	// Note: v2 policy related calls have their own service, so, for example, the
	// endpoint for retrieving whether IAMv1 or v2 is used, GetPolicyVersion, is
	// "/chef.automate.domain.authz.v2.Policies/GetPolicyVersion", and thus
	// exempt from this version check.

	// These methods skip the check, thought they are in the relevant service
	// definition:
	switch info.FullMethod {
	case "/chef.automate.domain.authz.Authorization/GetVersion":
		return handler(ctx, req)
	}

	v1Req := strings.HasPrefix(info.FullMethod, "/chef.automate.domain.authz.Authorization/")
	v2Req := strings.HasPrefix(info.FullMethod, "/chef.automate.domain.authz.v2.Authorization/")

	if v.version.Major == api_v2.Version_V2 && v1Req {
		st := status.New(codes.FailedPrecondition, "authz-service set to v2")
		st, err := st.WithDetails(&common.ErrorShouldUseV2{})
		if err != nil {
			return nil, status.Errorf(codes.Internal, "failed to add details to err: %v", err)
		}
		return nil, st.Err()
	}
	if v.version.Major == api_v2.Version_V1 && v2Req {
		st := status.New(codes.FailedPrecondition, "authz-service set to v1")
		st, err := st.WithDetails(&common.ErrorShouldUseV1{})
		if err != nil {
			return nil, status.Errorf(codes.Internal, "failed to add details to err: %v", err)
		}
		return nil, st.Err()
	}
	return handler(ctx, req)
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
