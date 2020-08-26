package handler

import (
	"context"

	log "github.com/sirupsen/logrus"

	service "github.com/chef/automate/api/external/event_feed"
	agReq "github.com/chef/automate/api/external/event_feed/request"
	agRes "github.com/chef/automate/api/external/event_feed/response"
	event_feed_api "github.com/chef/automate/api/interservice/event_feed"
	"github.com/chef/automate/components/automate-gateway/eventfeed"
)

// EventFeedServer stores client
type EventFeedServer struct {
	eventFeedAggregate *eventfeed.EventFeedAggregate
}

// NewEventFeedServer creates a new server instance
func NewEventFeedServer(feedClient event_feed_api.EventFeedServiceClient) *EventFeedServer {
	return &EventFeedServer{
		eventFeedAggregate: eventfeed.NewEventFeedAggregate(feedClient),
	}
}

// GetEventFeed returns a list of all Events
func (s *EventFeedServer) GetEventFeed(ctx context.Context,
	request *agReq.GetEventFeedRequest) (*agRes.GetEventFeedResponse, error) {
	log.WithFields(log.Fields{
		"request": request.String(),
		"func":    nameOfFunc(),
	}).Debug("rpc call")

	return s.eventFeedAggregate.CollectEventFeed(ctx, request)
}

// GetEventTypeCounts - gets event type counts
func (s *EventFeedServer) GetEventTypeCounts(ctx context.Context,
	request *agReq.GetEventTypeCountsRequest) (*agRes.GetEventTypeCountsResponse, error) {
	log.WithFields(log.Fields{
		"request": request.String(),
		"func":    nameOfFunc(),
	}).Debug("rpc call")

	return s.eventFeedAggregate.CollectEventTypeCounts(ctx, request)
}

// GetEventTaskCounts - gets event type counts
func (s *EventFeedServer) GetEventTaskCounts(ctx context.Context,
	request *agReq.GetEventTaskCountsRequest) (*agRes.GetEventTaskCountsResponse, error) {
	log.WithFields(log.Fields{
		"request": request.String(),
		"func":    nameOfFunc(),
	}).Debug("rpc call")

	return s.eventFeedAggregate.CollectEventTaskCounts(ctx, request)
}

// GetEventStringBuckets - gets the buckets for the guitar strings
func (s *EventFeedServer) GetEventStringBuckets(ctx context.Context,
	request *agReq.GetEventStringBucketsRequest) (*agRes.GetEventStringBucketsResponse, error) {
	log.WithFields(log.Fields{
		"request": request.String(),
		"func":    nameOfFunc(),
	}).Debug("rpc call")

	return s.eventFeedAggregate.CollectEventGuitarStrings(ctx, request)
}

// EventExport - downloading events as JSON or CSV
func (s *EventFeedServer) EventExport(*agReq.EventExportRequest, service.EventFeedService_EventExportServer) error {
	// Please see components/automate-gateway/services.go eventFeedExportHandler for implementation
	return nil
}
