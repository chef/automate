package grpc

import (
	"fmt"
	"net"

	log "github.com/sirupsen/logrus"
	"google.golang.org/grpc"
	"google.golang.org/grpc/reflection"

	"github.com/chef/automate/api/interservice/event_feed"
	"github.com/chef/automate/components/event-feed-service/pkg/config"
	"github.com/chef/automate/components/event-feed-service/pkg/server"
	"github.com/chef/automate/lib/grpc/health"
	"github.com/chef/automate/lib/grpc/secureconn"

	"github.com/olivere/elastic"
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

	grpcServer := NewGRPCServer(connFactory, c, esClient)
	return grpcServer.Serve(conn)
}

// NewGRPCServer returns a server that provides our services:
// * event feed
// * health
func NewGRPCServer(connFactory *secureconn.Factory, c *config.EventFeed,
	esClient *elastic.Client) *grpc.Server {
	grpcServer := connFactory.NewServer()

	eventFeedServer := server.New(esClient)

	event_feed.RegisterEventFeedServiceServer(grpcServer, eventFeedServer)

	health.RegisterHealthServer(grpcServer, eventFeedServer.Health())

	reflection.Register(grpcServer)

	return grpcServer
}
