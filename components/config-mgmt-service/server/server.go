//
//  Author:: Salim Afiune <afiune@chef.io>
//  Copyright:: Copyright 2017, Chef Software Inc.
//

package server

import (
	"net"

	log "github.com/sirupsen/logrus"
	"google.golang.org/grpc/reflection"

	gw "github.com/chef/automate/api/interservice/cfgmgmt/service"
	"github.com/chef/automate/components/config-mgmt-service/config"
	"github.com/chef/automate/components/config-mgmt-service/grpcserver"
	"github.com/chef/automate/lib/grpc/secureconn"
	"github.com/chef/automate/lib/tracing"
)

// StartGRPC starts a gRPC Server listening on the provided host and grpc port,
// it uses the backend URL to initialize the Pipelines
func StartGRPC(c *config.Service) error {
	log.WithFields(log.Fields{
		"uri": c.ListenAddress(),
	}).Info("Starting gRPC Server")

	conn, err := net.Listen("tcp", c.ListenAddress())
	if err != nil {
		log.WithFields(log.Fields{
			"error": err,
			"uri":   c.ListenAddress(),
		}).Fatal("TCP listen failed")
	}

	connFactory := secureconn.NewFactory(*c.GetServiceCerts())

	grpcServer := connFactory.NewServer(tracing.GlobalServerInterceptor())

	rootServer := grpcserver.NewCfgMgmtServer(c)
	gw.RegisterCfgMgmtServer(grpcServer, rootServer)

	// Register reflection service on gRPC server.
	reflection.Register(grpcServer)
	return grpcServer.Serve(conn)
}
