package server

import (
	"github.com/chef/automate/lib/grpc/health"
	"github.com/olivere/elastic"
)

// EventFeedServer is the interface to this component.
type EventFeedServer struct {
	health   *health.Service
	esClient *elastic.Client
}

// New creates a new EventFeedServer instance.
func New(esClient *elastic.Client) *EventFeedServer {
	return &EventFeedServer{
		health:   health.NewService(),
		esClient: esClient,
	}
}

// Health returns the servers embedded health check service
func (a *EventFeedServer) Health() *health.Service {
	return a.health
}
