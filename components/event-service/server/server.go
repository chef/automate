package server

import (
	"context"

	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/status"

	api "github.com/chef/automate/api/interservice/event"
	"github.com/chef/automate/components/event-service/config"
	"github.com/chef/automate/components/event-service/event"
	"github.com/chef/automate/lib/grpc/health"
	"github.com/chef/automate/lib/grpc/secureconn"
)

var (
	defaultRegistry = event.Registry{
		config.ScanJobCreatedEventName: []string{config.EventFeedEventName},
		config.ScanJobUpdatedEventName: []string{config.EventFeedEventName},
		config.ScanJobDeletedEventName: []string{config.EventFeedEventName},
		config.ProfileCreatedEventName: []string{config.EventFeedEventName},
		config.ProfileUpdatedEventName: []string{config.EventFeedEventName},
		config.ProfileDeletedEventName: []string{config.EventFeedEventName},
		config.NodeTerminatedEventName: []string{config.ConfigMgmtEventName},
	}
)

//----------  SERVER  ----------//

type Server struct {
	eventsService *event.EventService
	cfg           *config.EventConfig
	health        *health.Service
}

func New(cfg *config.EventConfig) (*Server, error) {
	f := secureconn.NewFactory(*cfg.ServiceCerts)

	server := &Server{
		eventsService: event.NewEventService(cfg, f),
		cfg:           cfg,
		health:        health.NewService(),
	}

	return server, server.eventsService.Start(defaultRegistry)
}

func (s *Server) Publish(ctx context.Context, req *api.PublishRequest) (*api.PublishResponse, error) {
	// translate event message from request into Event struct
	if msg := req.GetMsg(); msg != nil {
		s.eventsService.Publish(msg)
		return &api.PublishResponse{Success: true}, nil
	}

	return &api.PublishResponse{Success: false}, nil
}

// Should take a reference to another service's client, which should have a HandleEvent method on it
func (s *Server) Subscribe(ctx context.Context, req *api.SubscribeRequest) (*api.SubscribeResponse, error) {
	return &api.SubscribeResponse{}, nil
}

// Starts the automate-event event listener loop
func (s *Server) Start(ctx context.Context, req *api.StartRequest) (*api.StartResponse, error) {
	res := &api.StartResponse{}

	err := s.eventsService.Start(defaultRegistry)
	if err != nil {
		return res, status.Error(codes.Internal, err.Error())
	}

	return res, nil
}

// Stops the automate-event listener loop
func (s *Server) Stop(ctx context.Context, req *api.StopRequest) (*api.StopResponse, error) {
	return &api.StopResponse{}, s.eventsService.Stop(ctx)
}
