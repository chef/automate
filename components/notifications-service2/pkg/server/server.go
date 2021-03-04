package server

import (
	"context"
	"fmt"
	"net"

	log "github.com/sirupsen/logrus"
	"google.golang.org/grpc/reflection"

	notifications "github.com/chef/automate/api/interservice/notifications/service"
	"github.com/chef/automate/components/notifications-service2/pkg/config"
	"github.com/chef/automate/components/notifications-service2/pkg/storage"
	"github.com/chef/automate/lib/grpc/health"
	"github.com/chef/automate/lib/grpc/secureconn"
	"github.com/chef/automate/lib/version"
)

func RunGRPCServer(db storage.Client, c *config.Notifications) error {
	uri := fmt.Sprintf("%s:%d", c.Service.Host, c.Service.Port)
	log.WithFields(log.Fields{"uri": uri}).Info("Starting notifications-service gRPC Server")
	conn, err := net.Listen("tcp", uri)
	if err != nil {
		log.WithFields(log.Fields{"error": err}).Fatal("TCP listen failed")
		return err
	}

	connFactory := secureconn.NewFactory(*c.Certs,
		secureconn.WithVersionInfo(version.Version, version.GitSHA))
	grpcServer := connFactory.NewServer()

	notificationsServer := New(db)
	notifications.RegisterNotificationsServer(grpcServer, notificationsServer)

	reflection.Register(grpcServer)
	health.RegisterHealthServer(grpcServer, notificationsServer.Health())

	return grpcServer.Serve(conn)
}

type Server struct {
	health *health.Service
	db     storage.Client
}

func New(db storage.Client) *Server {
	return &Server{
		db:     db,
		health: health.NewService(),
	}
}

func (s *Server) Health() *health.Service {
	return s.health
}

func (s *Server) Notify(context.Context, *notifications.Event) (*notifications.Response, error) {
	return &notifications.Response{}, nil
}
func (s *Server) ValidateWebhook(context.Context, *notifications.URLValidationRequest) (*notifications.URLValidationResponse, error) {
	return &notifications.URLValidationResponse{}, nil
}
func (s *Server) Version(context.Context, *notifications.VersionRequest) (*notifications.VersionResponse, error) {
	return &notifications.VersionResponse{Version: version.Version}, nil
}
