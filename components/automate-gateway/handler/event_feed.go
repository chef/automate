package handler

import (
	"context"

	log "github.com/sirupsen/logrus"

	cmsService "github.com/chef/automate/api/interservice/cfgmgmt/service"
	agReq "github.com/chef/automate/components/automate-gateway/api/event_feed/request"
	agRes "github.com/chef/automate/components/automate-gateway/api/event_feed/response"
	"github.com/chef/automate/components/automate-gateway/eventfeed"
	ccFeed "github.com/chef/automate/components/compliance-service/api/automate-feed"
)

// EventFeedServer stores client
type EventFeedServer struct {
	eventFeedAggregate *eventfeed.EventFeedAggregate
}

// NewEventFeedServer creates a new server instance
func NewEventFeedServer(cfgMgmtClient cmsService.CfgMgmtClient, feedClient ccFeed.FeedServiceClient) *EventFeedServer {
	return &EventFeedServer{
		eventFeedAggregate: eventfeed.NewEventFeedAggregate(cfgMgmtClient, feedClient),
	}
}

// GetEventFeed returns a list of all Events
func (s *EventFeedServer) GetEventFeed(ctx context.Context,
	request *agReq.EventFilter) (*agRes.Events, error) {
	log.WithFields(log.Fields{
		"request": request.String(),
		"func":    nameOfFunc(),
	}).Debug("rpc call")

	return s.eventFeedAggregate.CollectEventFeed(ctx, request)
}

// GetEventTypeCounts - gets event type counts
func (s *EventFeedServer) GetEventTypeCounts(ctx context.Context,
	request *agReq.EventCountsFilter) (*agRes.EventCounts, error) {
	log.WithFields(log.Fields{
		"request": request.String(),
		"func":    nameOfFunc(),
	}).Debug("rpc call")

	return s.eventFeedAggregate.CollectEventTypeCounts(ctx, request)
}

// GetEventTaskCounts - gets event type counts
func (s *EventFeedServer) GetEventTaskCounts(ctx context.Context,
	request *agReq.EventCountsFilter) (*agRes.EventCounts, error) {
	log.WithFields(log.Fields{
		"request": request.String(),
		"func":    nameOfFunc(),
	}).Debug("rpc call")

	return s.eventFeedAggregate.CollectEventTaskCounts(ctx, request)
}

// GetEventStringBuckets - gets the buckets for the guitar strings
func (s *EventFeedServer) GetEventStringBuckets(ctx context.Context,
	request *agReq.EventStrings) (*agRes.EventStrings, error) {
	log.WithFields(log.Fields{
		"request": request.String(),
		"func":    nameOfFunc(),
	}).Debug("rpc call")

	return s.eventFeedAggregate.CollectEventGuitarStrings(ctx, request)
}
