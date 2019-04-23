package nodemanager

import (
	"context"
	"fmt"
	"net"
	"time"

	"github.com/pkg/errors"

	secretsapi "github.com/chef/automate/api/external/secrets"
	eventsapi "github.com/chef/automate/api/interservice/event"
	"github.com/chef/automate/components/compliance-service/utils/logging"
	"github.com/chef/automate/components/nodemanager-service/api/manager"
	managerserver "github.com/chef/automate/components/nodemanager-service/api/manager/server"
	"github.com/chef/automate/components/nodemanager-service/api/nodes"
	nodesserver "github.com/chef/automate/components/nodemanager-service/api/nodes/server"
	"github.com/chef/automate/components/nodemanager-service/config"
	"github.com/chef/automate/components/nodemanager-service/managers"
	"github.com/chef/automate/components/nodemanager-service/pgdb"
	"github.com/chef/automate/components/nodemanager-service/polling"

	"github.com/chef/automate/lib/grpc/health"
	"github.com/chef/automate/lib/grpc/secureconn"
	"github.com/chef/automate/lib/version"

	"github.com/chef/automate/lib/tracing"
	log "github.com/sirupsen/logrus"
	"google.golang.org/grpc"
	"google.golang.org/grpc/reflection"
)

type serviceState int

const (
	serviceStateUnknown = iota
	serviceStateStarting
	serviceStateStarted
)

var SERVICE_STATE serviceState

func Serve(conf config.Nodemanager, grpcBinding string) error {
	SERVICE_STATE = serviceStateUnknown
	logging.SetLogLevel(conf.Service.LogLevel)
	serviceCerts, err := conf.Service.ReadCerts()
	if err != nil {
		return errors.Wrap(err, "Unable to load service certificates")
	}
	connFactory := secureconn.NewFactory(*serviceCerts, secureconn.WithVersionInfo(
		version.BuildTime,
		version.GitSHA,
	))
	ctx := context.Background()
	SERVICE_STATE = serviceStateStarting

	return serve(ctx, &conf, connFactory)
}

func serve(ctx context.Context, config *config.Nodemanager, connFactory *secureconn.Factory) error {
	log.Infof("getting db connection")
	db, err := pgdb.New(&config.Postgres)
	if err != nil {
		log.WithError(err).Error("Creating postgres connection")
		return err
	}
	uri := fmt.Sprintf("%s:%d", config.Service.HostBind, config.Service.Port)
	log.WithFields(log.Fields{"uri": uri}).Info("Starting nodemanager gRPC Server")
	conn, err := net.Listen("tcp", uri)
	if err != nil {
		log.WithError(err).Error("TCP listen failed")
		return err
	}
	config.Secrets.Endpoint = fmt.Sprintf("%s:%d", config.Secrets.HostBind, config.Secrets.Port)

	grpcServer := newGRPCServer(db, connFactory, config)

	timeoutCtx, cancel := context.WithTimeout(ctx, 15*time.Second)
	defer cancel()

	// Set the secrets service client
	log.Infof("nodemanager setup, dialing secrets-service(%s)", config.Secrets.Endpoint)
	secretsConn, err := connFactory.DialContext(timeoutCtx, "secrets-service", config.Secrets.Endpoint, grpc.WithBlock())
	if err != nil {
		err = errors.New("nodemanager setup, error grpc dialing to secrets aborting...")
		return err
	}
	log.Debugf("nodemanager setup, getting a secrets service client")
	secretsClient := secretsapi.NewSecretsServiceClient(secretsConn)
	if secretsClient == nil {
		return fmt.Errorf("nodemanager setup, could not obtain secrets service client")
	}

	// Set the events service client
	log.Infof("nodemanager setup, getting an automate-event service client %s", config.EventConfig.Endpoint)
	eventConn, err := connFactory.DialContext(timeoutCtx, "event-service", config.EventConfig.Endpoint, grpc.WithBlock())
	if err != nil {
		err = errors.New("nodemanager setup, error grpc dialing to event-service aborting...")
		return err
	}
	log.Debug("nodemanager setup, getting a events service client")
	eventClient := eventsapi.NewEventServiceClient(eventConn)
	if eventClient == nil {
		return fmt.Errorf("nodemanager setup, could not obtain automate events service client")
	}
	// start polling for node mgrs, passing along the secrets client and event client
	// this function will call each of the manager polling functions that check the
	// state of nodes at the configured intervals
	polling.StartThePolls(ctx, config.Manager, db, secretsClient, eventClient)

	// create Automate node manager to handle manually added nodes. all manually created
	// nodes are assigned to the Automate node manager
	log.Debug("creating Automate node manager")
	_, err = managers.CreateManualNodeManager(db)
	if err != nil {
		err = errors.Wrap(err, "Couldn't create Automate manager")
		return err
	}
	SERVICE_STATE = serviceStateStarted
	return grpcServer.Serve(conn)
}

func newGRPCServer(db *pgdb.DB, connFactory *secureconn.Factory, config *config.Nodemanager) *grpc.Server {
	s := connFactory.NewServer(tracing.GlobalServerInterceptor())

	nodes.RegisterNodesServiceServer(s, nodesserver.New(db, connFactory, config.Secrets.Endpoint))
	manager.RegisterNodeManagerServiceServer(s, managerserver.New(db, connFactory, config.Secrets.Endpoint))

	healthServer := health.NewService()
	health.RegisterHealthServer(s, healthServer)

	reflection.Register(s)

	return s
}
