package grpc

import (
	"context"
	"fmt"
	"net"
	"time"

	"github.com/olivere/elastic"
	log "github.com/sirupsen/logrus"
	"google.golang.org/grpc"
	"google.golang.org/grpc/reflection"

	"github.com/chef/automate/api/interservice/data_lifecycle"
	"github.com/chef/automate/api/interservice/es_sidecar"
	"github.com/chef/automate/api/interservice/event_feed"
	"github.com/chef/automate/components/event-feed-service/pkg/config"
	"github.com/chef/automate/components/event-feed-service/pkg/migration"
	"github.com/chef/automate/components/event-feed-service/pkg/persistence"
	"github.com/chef/automate/components/event-feed-service/pkg/server"
	"github.com/chef/automate/lib/grpc/health"
	"github.com/chef/automate/lib/grpc/secureconn"
)

// Spawn starts a grpc server using the provided host and port.
func Spawn(c *config.EventFeed, connFactory *secureconn.Factory) error {

	uri := fmt.Sprintf("%s:%d", c.Service.Host, c.Service.Port)
	log.WithFields(log.Fields{"uri": uri}).Info("Starting event-feed-service gRPC Server")
	conn, err := net.Listen("tcp", uri)
	if err != nil {
		log.WithFields(log.Fields{"error": err}).Fatal("TCP listen failed")
		return err
	}

	esClient, err := elastic.NewClient(
		elastic.SetURL(c.ElasticSearchURL),
		elastic.SetSniff(false),
	)
	if err != nil {
		log.WithFields(log.Fields{
			"url":   c.ElasticSearchURL,
			"error": err.Error(),
		}).Error("could not connect to elasticsearch")
		return err
	}

	feedStore := persistence.NewFeedStore(esClient)

	migrator := migration.New(context.Background(), feedStore)

	err = migrator.InitializeStore()
	if err != nil {
		log.WithError(err).Error("Failed initializing elasticsearch")
		return err
	}

	timeoutCtx, cancel := context.WithTimeout(context.Background(), 60*time.Second)
	defer cancel()

	// Data Lifecycle Interface
	esSidecarConn, err := connFactory.DialContext(timeoutCtx, "es-sidecar-service",
		c.ESSidecarAddress, grpc.WithBlock())
	if err != nil {
		log.WithError(err).Error("Failed to create ES Sidecar connection")
		return err
	}
	defer func() {
		err := esSidecarConn.Close()
		if err != nil {
			log.WithError(err).Error("Failed closing ES sidecar connection")
		}
	}()

	grpcServer := newGRPCServer(connFactory, c, feedStore, esSidecarConn)

	return grpcServer.Serve(conn)
}

// NewGRPCServer returns a server that provides our services:
// * event feed
// * health
// * Data Lifecycle
func newGRPCServer(connFactory *secureconn.Factory, c *config.EventFeed,
	feedStore persistence.FeedStore, esSidecarConn *grpc.ClientConn) *grpc.Server {
	grpcServer := connFactory.NewServer()

	eventFeedServer := server.New(feedStore)

	event_feed.RegisterEventFeedServiceServer(grpcServer, eventFeedServer)

	health.RegisterHealthServer(grpcServer, eventFeedServer.Health())

	dataLifecycleServer := server.NewDataLifecycleManageableServer(
		es_sidecar.NewEsSidecarClient(esSidecarConn), c.PurgeEventFeedAfterDays)

	data_lifecycle.RegisterDataLifecycleManageableServer(grpcServer, dataLifecycleServer)

	reflection.Register(grpcServer)

	return grpcServer
}
