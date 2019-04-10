package server

import (
	"github.com/chef/automate/components/data-feed-service/config"
	"github.com/chef/automate/components/data-feed-service/service"
	"github.com/chef/automate/lib/grpc/health"
	log "github.com/sirupsen/logrus"
)

type Server struct {
	cfg    *config.DataFeedConfig
	health *health.Service
}

func New(cfg *config.DataFeedConfig) *Server {

	server := Server{
		cfg:    cfg,
		health: health.NewService(),
	}
	log.Debugf("data feed about to start polling service.")
	go service.Start(cfg)

	return &server
}
