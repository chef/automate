package server

import (
	"context"

	"github.com/sirupsen/logrus"

	api "github.com/chef/automate/api/interservice/event"
	"github.com/chef/automate/components/event-service/config"
	"github.com/chef/automate/components/event-service/event"
	"github.com/chef/automate/lib/grpc/health"
	"github.com/chef/automate/lib/grpc/secureconn"
)

// TODO: @gcp move to config pkg
const (
	// supported event types
	ScanJobCreated = "scanJobCreated"
	ScanJobUpdated = "scanJobUpdated"
	ScanJobDeleted = "scanJobDeleted"
	ProfileCreated = "profileCreated"
	ProfileUpdated = "profileUpdate"
	ProfileDeleted = "profileDeleted"
	NodeTerminated = "nodeTerminated"
)

//----------  SERVER  ----------//

type Server struct {
	eventsService *event.Events
	isStarted     bool
	cfg           *config.EventConfig
	health        *health.Service
}

func New(cfg *config.EventConfig) *Server {
	f := secureconn.NewFactory(*cfg.ServiceCerts)

	server := Server{
		eventsService: event.NewEvents(cfg, f),
		isStarted:     false,
		cfg:           cfg,
		health:        health.NewService(),
	}

	go server.eventsService.Start(server.initRegistry())
	server.isStarted = true
	return &server
}

func (s *Server) initRegistry() map[string][]string {

	registry := make(map[string][]string)
	registry[ScanJobCreated] = []string{config.EVENT_FEED}
	registry[ScanJobUpdated] = []string{config.EVENT_FEED}
	registry[ScanJobDeleted] = []string{config.EVENT_FEED}
	registry[ProfileCreated] = []string{config.EVENT_FEED}
	registry[ProfileUpdated] = []string{config.EVENT_FEED}
	registry[ProfileDeleted] = []string{config.EVENT_FEED}
	registry[NodeTerminated] = []string{config.CFG_KEY}
	logrus.Debug("Registry of events to handler types initialized...")
	return registry
}

func (s *Server) Publish(ctx context.Context, req *api.PublishRequest) (*api.PublishResponse, error) {
	// translate event message from request into Event struct
	if req.GetMsg() != nil {
		s.eventsService.Publish(req.GetMsg())
		return &api.PublishResponse{Success: true}, nil
	}

	return &api.PublishResponse{Success: false}, nil
}

// Should take a reference to another service's client, which should have a HandleEvent method on it
func (s *Server) Subscribe(ctx context.Context, req *api.SubscribeRequest) (*api.SubscribeResponse, error) {
	return nil, nil
}

// Starts the automate-event event listener loop
func (s *Server) Start(ctx context.Context, req *api.StartRequest) (*api.StartResponse, error) {
	if !s.isStarted {
		s.eventsService.Start(s.initRegistry())
		s.isStarted = true
	}
	return nil, nil
}

// Stops the automate-event listener loop
func (s *Server) Stop(ctx context.Context, req *api.StopRequest) (*api.StopResponse, error) {
	if s.isStarted {
		s.eventsService.Stop()
	}
	return nil, nil
}
