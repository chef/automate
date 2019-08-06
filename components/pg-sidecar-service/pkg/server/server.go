package server

import (
	"context"
	"encoding/json"
	"net"
	"os"
	"os/signal"
	"syscall"

	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"
	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/reflection"
	"google.golang.org/grpc/status"

	api "github.com/chef/automate/api/interservice/pg_sidecar"
	"github.com/chef/automate/components/pg-sidecar-service/pkg/pgw"
	"github.com/chef/automate/lib/grpc/health"
	"github.com/chef/automate/lib/grpc/secureconn"
	"github.com/chef/automate/lib/io/fileutils"
	"github.com/chef/automate/lib/platform"
	"github.com/chef/automate/lib/tracing"
	"github.com/chef/automate/lib/version"
)

// StartGRPC takes a context and server.Config, initializes a new Server and
// starts listening on the configured address
func StartGRPC(ctx context.Context, cfg *Config) error {
	tlsOpts, err := cfg.ReadCerts()
	if err != nil {
		return errors.Wrap(err, "failed to load SSL key/cert files")
	}

	platformConfig, err := platform.ConfigFromEnvironment()
	if err != nil {
		return errors.Wrap(err, "failed to load platform config")
	}

	listener, err := net.Listen("tcp", cfg.ListenAddress())
	if err != nil {
		return errors.Wrapf(err, "failed to listen on address %s", cfg.ListenAddress())
	}

	connFactory := secureconn.NewFactory(*tlsOpts, secureconn.WithVersionInfo(
		version.BuildTime,
		version.GitSHA,
	))
	grpcServer := connFactory.NewServer(tracing.GlobalServerInterceptor())
	sidecarSvr := NewPGSidecarServer(
		WithPGSidecarServerConfig(cfg),
		WithPGSidecarServerPlatformConfig(platformConfig),
	)
	if err = sidecarSvr.Start(); err != nil {
		return err
	}

	api.RegisterPGSidecarServer(grpcServer, sidecarSvr)
	health.RegisterHealthServer(grpcServer, sidecarSvr.health)
	reflection.Register(grpcServer)

	// Attempt to gracefully handle shutdowns
	ch := make(chan os.Signal, 1)
	signal.Notify(ch, syscall.SIGTERM, syscall.SIGINT)
	go func() {
		sig := <-ch
		logrus.WithField("signal", sig).Info("Shutting down")
		// GracefulStop will stop new connections on our listener and process
		// in-flight RPCs
		grpcServer.GracefulStop()
		// All RPCs should be completed by now but we'll ensure any procedures
		// that have been queued are also processed.
		sidecarSvr.spr.Stop()
		os.Exit(0)
	}()

	return grpcServer.Serve(listener)
}

// PGSidecarServerOpt is a functional option for configuring the PGSidecarServer
type PGSidecarServerOpt func(*PGSidecarServer)

// WithPGSidecarServerConfig sets the PGSidecarServer config
func WithPGSidecarServerConfig(cfg *Config) PGSidecarServerOpt {
	return func(svr *PGSidecarServer) {
		svr.cfg = cfg
	}
}

// WithPGSidecarServerPlatformConfig sets the platform config used by the server
func WithPGSidecarServerPlatformConfig(platformConfig *platform.Config) PGSidecarServerOpt {
	return func(svr *PGSidecarServer) {
		svr.platformConfig = platformConfig
	}
}

// NewPGSidecarServer takes functional options and returns a pointer to a new
// PGSidecarServer
func NewPGSidecarServer(opts ...PGSidecarServerOpt) *PGSidecarServer {
	svr := &PGSidecarServer{
		health: health.NewService(),
	}

	for _, opt := range opts {
		opt(svr)
	}

	return svr
}

// PGSidecarServer is the implementation of the pg-sidecar-server
type PGSidecarServer struct {
	cfg            *Config
	health         *health.Service
	spr            *pgw.SerialProcedureRunner
	platformConfig *platform.Config
}

// SetPublicSchemaRole applies a role to public and sqitch schemas
func (p *PGSidecarServer) SetPublicSchemaRole(ctx context.Context, req *api.SetPublicSchemaRoleReq) (*api.SetPublicSchemaRoleRes, error) {
	res := &api.SetPublicSchemaRoleRes{}

	client, err := pgw.NewClient(
		pgw.WithPlatformConfig(p.platformConfig),
		pgw.WithDb(req.Db),
	)
	if err != nil {
		return res, status.New(codes.FailedPrecondition, err.Error()).Err()
	}
	defer fileutils.LogClose(client, logrus.StandardLogger(), "failed to close client")

	proc, err := p.queueSerialProcedure(
		ctx,
		func() error { return client.SetPublicSchemaRole(req.Role) },
	)
	if err != nil {
		return res, err
	}
	defer proc.Close()

	return res, proc.Wait()
}

// AlterRole alters a role with request options
func (p *PGSidecarServer) AlterRole(ctx context.Context, req *api.AlterRoleReq) (*api.AlterRoleRes, error) {
	res := &api.AlterRoleRes{}
	var b []byte

	client, err := p.newClient()
	if err != nil {
		return res, status.New(codes.FailedPrecondition, err.Error()).Err()
	}
	defer fileutils.LogClose(client, logrus.StandardLogger(), "failed to close client")

	query := pgw.NewAlterRoleQuery()
	b, err = json.Marshal(req.With)
	if err != nil {
		return res, status.New(codes.InvalidArgument, err.Error()).Err()
	}

	err = json.Unmarshal(b, query)
	if err != nil {
		return res, status.New(codes.InvalidArgument, err.Error()).Err()
	}

	if query.String() == "" {
		return res, status.New(codes.InvalidArgument, "failed to provide ALTER ROLE options").Err()
	}

	query.Role = req.GetRole()

	proc, err := p.queueSerialProcedure(
		ctx,
		func() error { return client.AlterRole(query) },
	)
	if err != nil {
		return res, err
	}
	defer proc.Close()

	return res, proc.Wait()
}

// CreateDB creates a database
func (p *PGSidecarServer) CreateDB(ctx context.Context, req *api.CreateDBReq) (*api.CreateDBRes, error) {
	res := &api.CreateDBRes{}

	client, err := p.newClient()
	if err != nil {
		return res, status.New(codes.FailedPrecondition, err.Error()).Err()
	}
	defer fileutils.LogClose(client, logrus.StandardLogger(), "failed to close client")

	proc, err := p.queueSerialProcedure(
		ctx,
		func() error { return client.CreateDB(req.Db, req.User) },
	)
	if err != nil {
		return res, err
	}
	defer proc.Close()

	return res, proc.Wait()
}

// CreateExtension creates an extension for a given database
func (p *PGSidecarServer) CreateExtension(ctx context.Context, req *api.CreateExtensionReq) (*api.CreateExtensionRes, error) {
	res := &api.CreateExtensionRes{}

	client, err := pgw.NewClient(
		pgw.WithPlatformConfig(p.platformConfig),
		pgw.WithDb(req.Db),
	)
	if err != nil {
		return res, status.New(codes.FailedPrecondition, err.Error()).Err()
	}
	defer fileutils.LogClose(client, logrus.StandardLogger(), "failed to close client")

	proc, err := p.queueSerialProcedure(
		ctx,
		func() error { return client.CreateExtension(req.Ext) },
	)
	if err != nil {
		return res, err
	}
	defer proc.Close()

	return res, proc.Wait()
}

// DeploySqitch runs sqitch for a given database, user and schema directory
func (p *PGSidecarServer) DeploySqitch(ctx context.Context, req *api.DeploySqitchReq) (*api.DeploySqitchRes, error) {
	res := &api.DeploySqitchRes{}

	client, err := p.newClient()
	if err != nil {
		return res, status.New(codes.FailedPrecondition, err.Error()).Err()
	}
	defer fileutils.LogClose(client, logrus.StandardLogger(), "failed to close client")

	proc, err := p.queueSerialProcedure(
		ctx,
		func() error { return client.DeploySqitch(req.Db, req.Dir, req.User) },
	)
	if err != nil {
		return res, err
	}
	defer proc.Close()

	return res, proc.Wait()
}

// MigrateTables migrates from one database to the other
func (p *PGSidecarServer) MigrateTables(ctx context.Context, req *api.MigrateTablesReq) (*api.MigrateTablesRes, error) {
	res := &api.MigrateTablesRes{}

	client, err := p.newClient()
	if err != nil {
		return res, status.New(codes.FailedPrecondition, err.Error()).Err()
	}
	defer fileutils.LogClose(client, logrus.StandardLogger(), "failed to close client")

	proc, err := p.queueSerialProcedure(
		ctx,
		func() error {
			return client.MigrateTables(
				req.FromDb,
				req.ToDb,
				req.Table,
				req.ImportUser,
				req.FailIfSrcDbMissing,
				req.SkipDbCreate,
			)
		},
	)
	if err != nil {
		return res, err
	}
	defer proc.Close()

	err = proc.Wait()
	if err != nil {
		if err == pgw.ErrSrcDbMissing {
			// Make sure to annotate the status with extra details if the
			// source database is missing so that clients can determine how
			// handle the error.
			st := status.New(codes.FailedPrecondition, err.Error())
			st, _ = st.WithDetails(&api.ErrorDetails{Code: api.ErrorDetails_SrcDbMissing})
			return res, st.Err()
		}
	}

	return res, err
}

// DropTables removes tables if they exist
func (p *PGSidecarServer) DropTables(ctx context.Context, req *api.DropTablesReq) (*api.DropTablesRes, error) {
	res := &api.DropTablesRes{}

	if len(req.Tables) == 0 {
		return res, nil
	}

	client, err := pgw.NewClient(
		pgw.WithPlatformConfig(p.platformConfig),
		pgw.WithDb(req.Db),
	)
	if err != nil {
		return res, status.New(codes.FailedPrecondition, err.Error()).Err()
	}
	defer fileutils.LogClose(client, logrus.StandardLogger(), "failed to close client")

	query := pgw.NewDropTablesQuery()
	query.Tables = req.Tables
	query.Cascade = req.Cascade

	proc, err := p.queueSerialProcedure(
		ctx,
		func() error {
			return client.DropTables(query)
		},
	)
	if err != nil {
		return res, err
	}
	defer proc.Close()

	return res, proc.Wait()
}

// RenameDB renames a database
func (p *PGSidecarServer) RenameDB(ctx context.Context, req *api.RenameDBReq) (*api.RenameDBRes, error) {
	res := &api.RenameDBRes{}

	client, err := p.newClient()
	if err != nil {
		return res, status.New(codes.FailedPrecondition, err.Error()).Err()
	}
	defer fileutils.LogClose(client, logrus.StandardLogger(), "failed to close client")

	proc, err := p.queueSerialProcedure(
		ctx,
		func() error { return client.RenameDB(req.ToDb, req.FromDb) },
	)
	if err != nil {
		return res, err
	}
	defer proc.Close()

	return res, proc.Wait()
}

// Start initializes the PGSidecarServer
func (p *PGSidecarServer) Start() error {
	// Ensure that we can connect to our database
	client, err := p.newClient()
	if err != nil {
		return err
	}
	defer fileutils.LogClose(client, logrus.StandardLogger(), "failed to close client")

	// Start the serial procedure runner
	p.spr = pgw.NewSerialProcedureRunner()
	p.spr.Start()

	return nil
}

func (p *PGSidecarServer) newClient() (*pgw.Client, error) {
	return pgw.NewClient(
		pgw.WithPlatformConfig(p.platformConfig),
	)
}

func (p *PGSidecarServer) queueSerialProcedure(ctx context.Context, proc func() error) (pgw.Procedure, error) {
	var err error
	procedure := pgw.NewProcedure(ctx, proc)

	// Try and queue the procedure. If we can't push into the runners queue then
	// we'll return a retriable status error
	select {
	case p.spr.C.C <- procedure:
	default:
		err = status.New(codes.Unavailable, "serial procedure queue is full").Err()
	}

	return procedure, err
}
