package grpc

import (
	"fmt"
	"net"

	log "github.com/sirupsen/logrus"
	"google.golang.org/grpc"
	"google.golang.org/grpc/reflection"

	"github.com/chef/automate/components/event-feed-service/pkg/config"
	// "github.com/chef/automate/lib/grpc/health"
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

	grpcServer := NewGRPCServer(connFactory, c)
	return grpcServer.Serve(conn)
}

// NewGRPCServer returns a server that provides our services:
// * applications
// * health
func NewGRPCServer(connFactory *secureconn.Factory, c *config.EventFeed) *grpc.Server {
	grpcServer := connFactory.NewServer()

	log.Info("Creating Server and starting it")
	// applicationsServer := server.New(c.GetStorage())
	// applications.RegisterApplicationsServiceServer(grpcServer, applicationsServer)

	// health.RegisterHealthServer(grpcServer, applicationsServer.Health())

	reflection.Register(grpcServer)

	return grpcServer
}
