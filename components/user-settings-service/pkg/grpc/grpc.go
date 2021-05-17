package grpc

import (
	"github.com/chef/automate/api/interservice/user_settings"
	"github.com/chef/automate/lib/grpc/health"

	"fmt"
	"github.com/chef/automate/components/user-settings-service/pkg/config"
	uss "github.com/chef/automate/components/user-settings-service/pkg/server"
	"github.com/chef/automate/lib/grpc/secureconn"
	"github.com/pkg/errors"
	log "github.com/sirupsen/logrus"
	"google.golang.org/grpc/reflection"
	"net"
)

func Spawn(c *config.UserSettings, connFactory *secureconn.Factory) error {
	grpcServer := connFactory.NewServer()
	userSettingsServer := uss.New(c.GetStorage())

	user_settings.RegisterUserSettingsServiceServer(grpcServer, userSettingsServer)
	health.RegisterHealthServer(grpcServer, health.NewService())

	reflection.Register(grpcServer)

	listenAddr := fmt.Sprintf("%s:%d", c.Service.Host, c.Service.Port)
	log.WithFields(log.Fields{"uri": listenAddr}).Info("Starting user-settings-service gRPC Server")
	lis, err := net.Listen("tcp", listenAddr)
	if err != nil {
		panic(errors.Wrapf(err, "failed to listen on port %s", listenAddr))
	}

	return grpcServer.Serve(lis)
}
