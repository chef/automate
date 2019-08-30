package grpc

import (
	"fmt"
	"net"

	log "github.com/sirupsen/logrus"
	"google.golang.org/grpc/reflection"

	"github.com/chef/automate/api/external/applications"
	"github.com/chef/automate/components/applications-service/pkg/config"
	"github.com/chef/automate/components/applications-service/pkg/server"
	"github.com/chef/automate/lib/grpc/health"
	"github.com/chef/automate/lib/grpc/secureconn"
)

// Spawn starts a grpc server using the provided host and port.
func Spawn(c *config.Applications, connFactory *secureconn.Factory) error {

	uri := fmt.Sprintf("%s:%d", c.Service.Host, c.Service.Port)
	log.WithFields(log.Fields{"uri": uri}).Info("Starting applications-service gRPC Server")
	conn, err := net.Listen("tcp", uri)
	if err != nil {
		log.WithFields(log.Fields{"error": err}).Fatal("TCP listen failed")
		return err
	}

	jobsMgr, err := server.ConnectToJobsManager(&c.Jobs, connFactory)
	if err != nil {
		log.WithError(err).Fatal("Failed to connect to upstream cereal service")
		return err
	}

	scheduler := server.NewJobScheduler(jobsMgr)
	if err := scheduler.Setup(); err != nil {
		log.WithError(err).Fatal("Failed to configure periodic jobs in upstream cereal service")
		return err
	}

	grpcServer := connFactory.NewServer()

	applicationsServer := server.New(c.GetStorage(), c.GetIngester(), scheduler)
	applications.RegisterApplicationsServiceServer(grpcServer, applicationsServer)

	health.RegisterHealthServer(grpcServer, applicationsServer.Health())

	reflection.Register(grpcServer)

	jobRunners := server.NewJobRunnerSet(applicationsServer)
	if err := jobRunners.Start(jobsMgr); err != nil {
		log.WithError(err).Fatal("Failed to start runners for periodic jobs")
		return err
	}

	return grpcServer.Serve(conn)
}
