package grpc

import (
	"context"
	"fmt"
	"net"
	"time"

	"github.com/pkg/errors"
	log "github.com/sirupsen/logrus"
	"google.golang.org/grpc"
	libgrpc "google.golang.org/grpc"
	"google.golang.org/grpc/reflection"
	elastic "gopkg.in/olivere/elastic.v6"

	"github.com/chef/automate/api/interservice/data_lifecycle"
	"github.com/chef/automate/api/interservice/es_sidecar"
	"github.com/chef/automate/api/interservice/event_feed"
	"github.com/chef/automate/components/event-feed-service/pkg/config"
	"github.com/chef/automate/components/event-feed-service/pkg/migration"
	"github.com/chef/automate/components/event-feed-service/pkg/persistence"
	"github.com/chef/automate/components/event-feed-service/pkg/server"
	"github.com/chef/automate/lib/cereal"
	cereal_grpc "github.com/chef/automate/lib/cereal/grpc"
	"github.com/chef/automate/lib/datalifecycle/purge"
	"github.com/chef/automate/lib/grpc/health"
	"github.com/chef/automate/lib/grpc/secureconn"
)

// Spawn starts a gRPC server using the provided configuration
func Spawn(c *config.EventFeed, connFactory *secureconn.Factory) error {

	uri := fmt.Sprintf("%s:%d", c.Service.Host, c.Service.Port)
	log.WithField("uri", uri).Info("Starting event-feed-service gRPC Server")
	conn, err := net.Listen("tcp", uri)
	if err != nil {
		return errors.Wrap(err, "starting TCP listener")
	}

	esClient, err := elastic.NewClient(
		elastic.SetURL(c.ElasticSearchURL),
		elastic.SetSniff(false),
	)
	if err != nil {
		return errors.Wrapf(err, "connecting to elasticsearch (%s)", c.ElasticSearchURL)
	}

	feedStore := persistence.NewFeedStore(esClient)

	migrator := migration.New(context.Background(), feedStore)

	err = migrator.InitializeStore()
	if err != nil {
		return errors.Wrap(err, "initializing elasticsearch")
	}

	timeoutCtx, cancel := context.WithTimeout(context.Background(), 60*time.Second)
	defer cancel()

	esSidecarConn, err := connFactory.DialContext(
		timeoutCtx, "es-sidecar-service", c.ESSidecarAddress, libgrpc.WithBlock(),
	)
	if err != nil {
		return errors.Wrap(err, "initializing es-sidecar-service backend connection")
	}
	defer esSidecarConn.Close() //nolint errcheck

	esSidecarClient := es_sidecar.NewEsSidecarClient(esSidecarConn)

	cerealConn, err := connFactory.DialContext(
		timeoutCtx, "cereal-service", c.Cereal.Address,
		libgrpc.WithBlock(), libgrpc.WithMaxMsgSize(64*1024*1024),
	)
	if err != nil {
		return errors.Wrap(err, "initializing cereal-service backend connection")
	}
	defer cerealConn.Close() // nolint errcheck
	cerealBackend := cereal_grpc.NewGrpcBackendFromConn("event-feed-service", cerealConn)

	jobManager, cleanup, err := newJobManager(c, esSidecarClient, cerealBackend)
	if err != nil {
		return errors.Wrap(err, "initializing job manager")
	}
	defer cleanup()

	purgeServer, err := purge.NewServer(
		jobManager,
		server.PurgeScheduleName,
		server.PurgeWorkflowName,
		server.DefaultPurgePolicies,
		purge.WithServerEsSidecarClient(esSidecarClient),
	)
	if err != nil {
		return errors.Wrap(err, "initializing purge server")
	}

	grpcServer := newGRPCServer(connFactory, c, feedStore, purgeServer)

	return grpcServer.Serve(conn)
}

// newGRPCServer returns a server that provides our services:
// * event feed
// * health
// * data lifecycle purge
func newGRPCServer(
	connFactory *secureconn.Factory,
	c *config.EventFeed,
	feedStore persistence.FeedStore,
	purgeServer *purge.Server) *grpc.Server {

	grpcServer := connFactory.NewServer()

	eventFeedServer := server.New(feedStore)
	event_feed.RegisterEventFeedServiceServer(grpcServer, eventFeedServer)

	health.RegisterHealthServer(grpcServer, eventFeedServer.Health())

	data_lifecycle.RegisterPurgeServer(grpcServer, purgeServer)

	reflection.Register(grpcServer)

	return grpcServer
}

// newJobManager returns a cereal manager to handle scheduled jobs
func newJobManager(
	c *config.EventFeed,
	esSidecarClient es_sidecar.EsSidecarClient,
	cerealBackend cereal.Driver) (*cereal.Manager, func(), error) {

	var (
		man     *cereal.Manager
		cleanup = func() {}
	)

	jobManager, err := cereal.NewManager(cerealBackend)
	if err != nil {
		return man, cleanup, errors.Wrap(err, "creating job manager")
	}

	err = purge.ConfigureManager(
		jobManager,
		server.PurgeWorkflowName,
		purge.WithTaskEsSidecarClient(esSidecarClient),
	)
	if err != nil {
		return man, cleanup, errors.Wrap(err, "configuring purge workflow")
	}

	err = server.ConfigureJobManager(jobManager, c)
	if err != nil {
		return man, cleanup, errors.Wrap(err, "migrating purge policies")
	}

	err = jobManager.Start(context.Background())
	if err != nil {
		return man, cleanup, errors.Wrap(err, "starting job manager")
	}

	cleanup = func() {
		_ = jobManager.Stop()
	}

	return jobManager, cleanup, nil
}
