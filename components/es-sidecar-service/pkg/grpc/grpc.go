package grpc

import (
	"fmt"
	"net"

	log "github.com/sirupsen/logrus"
	"google.golang.org/grpc/reflection"

	api "github.com/chef/automate/api/interservice/es_sidecar"
	"github.com/chef/automate/components/es-sidecar-service/pkg/config"
	"github.com/chef/automate/components/es-sidecar-service/pkg/elastic"
	"github.com/chef/automate/components/es-sidecar-service/pkg/server"
	"github.com/chef/automate/lib/grpc/secureconn"
)

// Spawn starts a grpc server using the provided host and port.
func Spawn(config *config.Service) error {
	tlsOpts, err := config.ReadCerts()
	if err != nil {
		log.WithFields(log.Fields{
			"error": err.Error(),
		}).Fatal("Failed to load SSL key/cert files")
	}
	connFactory := secureconn.NewFactory(*tlsOpts)

	es, err := elastic.New(config.ElasticsearchURL)
	if err != nil {
		log.WithFields(log.Fields{
			"url":   config.ElasticsearchURL,
			"error": err.Error(),
		}).Error("could not connect to elasticsearch")
		return err
	}

	uri := fmt.Sprintf("%s:%d", config.Host, config.Port)
	log.WithFields(log.Fields{"uri": uri}).Info("Starting es-sidecar-service gRPC Server")
	conn, err := net.Listen("tcp", uri)
	if err != nil {
		log.WithFields(log.Fields{"error": err}).Fatal("TCP listen failed")
		return err
	}

	grpcServer := connFactory.NewServer()

	purgeServer := server.NewEsSidecarServer(es, &config.BackupsConfig)
	api.RegisterEsSidecarServer(grpcServer, purgeServer)
	reflection.Register(grpcServer)
	return grpcServer.Serve(conn)
}
