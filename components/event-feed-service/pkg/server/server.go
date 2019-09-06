package server

import (
	"context"
	"time"

	"github.com/golang/protobuf/ptypes"
	"github.com/pkg/errors"
	log "github.com/sirupsen/logrus"
	"google.golang.org/grpc/codes"

	automate_event "github.com/chef/automate/api/interservice/event"
	"github.com/chef/automate/api/interservice/event_feed"
	e "github.com/chef/automate/components/event-feed-service/pkg/errors"
	"github.com/chef/automate/components/event-feed-service/pkg/feed"
	"github.com/chef/automate/components/event-feed-service/pkg/persistence"
	"github.com/chef/automate/lib/grpc/health"
)

// EventFeedServer is the interface to this component.
type EventFeedServer struct {
	health      *health.Service
	feedService *FeedService
}

// New creates a new EventFeedServer instance.
func New(feedStore persistence.FeedStore) *EventFeedServer {
	return &EventFeedServer{
		health:      health.NewService(),
		feedService: NewFeedService(feedStore),
	}
}

// Health returns the servers embedded health check service
func (eventFeedServer *EventFeedServer) Health() *health.Service {
	return eventFeedServer.health
}

// TODO: @gcp refactor so that param validations & resulting grpc errors thrown here
// TODO: @gcp consistent behavior on err w/r/t returning either empty result set or nil
func (eventFeedServer *EventFeedServer) GetFeed(ctx context.Context,
	request *event_feed.FeedRequest) (*event_feed.FeedResponse, error) {
	var (
		feedEntries []*feed.FeedEntry
		hits        int64
		err         error
	)

	logctx := log.WithFields(log.Fields{
		"request": request.String(),
		"rpc":     "GetFeed",
	})

	if feedEntries, hits, err = eventFeedServer.feedService.GetFeed(request); err != nil {
		logctx.WithError(err).Warn("getting feed")
		return nil, err
	}

	fe, err := FromInternalFormatToList(feedEntries)
	if err != nil {
		logctx.WithError(err).Warn("formating feed entries")
		return nil, err
	}

	return &event_feed.FeedResponse{
		FeedEntries:  fe,
		TotalEntries: hits,
	}, nil
}

func (eventFeedServer *EventFeedServer) GetFeedSummary(ctx context.Context,
	request *event_feed.FeedSummaryRequest) (*event_feed.FeedSummaryResponse, error) {

	logctx := log.WithFields(log.Fields{
		"request": request.String(),
		"rpc":     "GetFeedSummary",
	})

	// Date Range
	startTime, endTime, err := feed.ValidateMillisecondDateRange(request.Start, request.End)
	if err != nil {
		logctx.WithError(err).Warn("validating date range")
		return &event_feed.FeedSummaryResponse{}, e.GrpcErrorFromErr(codes.InvalidArgument, err)
	}

	fs, err := eventFeedServer.feedService.GetFeedSummary(request.CountCategory, request.Filters,
		startTime, endTime)
	if err != nil {
		logctx.WithError(err).Warn("getting feed summary")
		return &event_feed.FeedSummaryResponse{}, e.GrpcErrorFromErr(codes.Internal, err)
	}

	// from internal to external feed summary format
	return fromInternalFormatSummary(fs), nil
}

// GetFeedTimeline - feed timeline consists of multiple lines, one for each
// type of action: create = 2 update = 1 delete = 0. This function gets the
// data for each line of the feed timeline (the "guitar strings" graph).
//
// ----------------------------------------------------------------------------
// |         Jan 28         |         Jan 29         |         Jan 30         |
// |_____________b__________|________________________|_____________i__________|'create'
// |________________________|_____________c__________|________________________|'update'
// |________________________|________________________|________________________|'delete'
// ----------------------------------------------------------------------------
func (eventFeedServer *EventFeedServer) GetFeedTimeline(ctx context.Context,
	request *event_feed.FeedTimelineRequest) (*event_feed.FeedTimelineResponse, error) {
	logctx := log.WithFields(log.Fields{
		"request": request.String(),
		"rpc":     "GetFeedTimeline",
	})

	var (
		actionsMap  = []string{"create", "update", "delete"}
		actionLines = make([]*event_feed.ActionLine, len(actionsMap))
	)

	err := validateFeedTimelineRequest(request)
	if err != nil {
		logctx.WithError(err).Warn("validating request")
		return &event_feed.FeedTimelineResponse{}, e.GrpcErrorFromErr(codes.InvalidArgument, err)
	}

	// Create three lines, one for each action
	for i, action := range actionsMap {
		actionLine, err := eventFeedServer.feedService.GetActionLine(request.Filters, request.Start,
			request.End, request.Timezone, int(request.Interval), action)
		if err != nil {
			logctx.WithError(err).Warn("getting action line")
			return &event_feed.FeedTimelineResponse{}, e.GrpcErrorFromErr(codes.Internal, err)
		}
		actionLines[i] = fromInternalFormatActionLine(actionLine)
	}

	return &event_feed.FeedTimelineResponse{
		Start:       request.Start,
		End:         request.End,
		Interval:    request.Interval,
		ActionLines: actionLines,
	}, nil
}

func (eventFeedServer *EventFeedServer) HandleEvent(ctx context.Context,
	request *automate_event.EventMsg) (*automate_event.EventResponse, error) {
	logctx := log.WithFields(log.Fields{
		"request": request.String(),
		"rpc":     "HandleEvent",
	})

	logctx.Debug("handling event")

	response, err := eventFeedServer.feedService.HandleEvent(request)
	if err != nil {
		logctx.WithError(err).Warn("handling event")
		return response, err
	}
	return response, nil
}

func FromInternalFormatToList(entries []*feed.FeedEntry) ([]*event_feed.FeedEntry, error) {
	tl := make([]*event_feed.FeedEntry, len(entries))

	for i, entry := range entries {
		e, err := fromInternalFormat(entry)
		if err != nil {
			return nil, err
		}
		tl[i] = e
	}

	return tl, nil
}

func validateFeedTimelineRequest(req *event_feed.FeedTimelineRequest) error {
	// Validate Timezone
	if req.Timezone == "" {
		return e.GrpcError(codes.InvalidArgument, "timezone must be provided")
	}

	_, err := time.LoadLocation(req.Timezone)
	if err != nil {
		return e.GrpcErrorFromErr(codes.InvalidArgument, err)
	}

	// Validate Interval
	if req.Interval <= 0 || req.Interval > 24 {
		return e.GrpcError(codes.InvalidArgument,
			"time interval must be greater than 0 and less than or equal to 24 hours")
	}

	if 24%req.Interval != 0 {
		return e.GrpcError(codes.InvalidArgument, "time interval must devide 24")
	}

	// Validate Date Range
	if !feed.ValidateDateRange(req.Start, req.End) {
		return e.GrpcError(codes.InvalidArgument, "invalid start/end time. (format: YYYY-MM-DD)")
	}

	return nil
}

func fromInternalFormat(entry *feed.FeedEntry) (*event_feed.FeedEntry, error) {
	pubTs, err := ptypes.TimestampProto(entry.Published)
	if err != nil {
		return nil, errors.Wrapf(err, "invalid published timestamp: %s", entry.Published.String())
	}

	createTs, err := ptypes.TimestampProto(entry.Created)
	if err != nil {
		return nil, errors.Wrapf(err, "invalid created timestamp: %s", entry.Created.String())
	}

	return &event_feed.FeedEntry{
		ID:                   entry.ID,
		EventType:            entry.EventType,
		FeedType:             entry.FeedType,
		Tags:                 entry.Tags,
		SourceEventPublished: pubTs,
		Created:              createTs,
		Producer: &event_feed.Producer{
			ID:         entry.ProducerID,
			Name:       entry.ProducerName,
			ObjectType: entry.ProducerObjectType,
			PTags:      entry.ProducerTags,
		},
		Actor: &event_feed.Actor{
			ID:         entry.ActorID,
			Name:       entry.ActorName,
			ObjectType: entry.ActorObjectType,
		},
		Verb: entry.Verb,
		Object: &event_feed.Object{
			ID:         entry.ObjectID,
			Name:       entry.ObjectName,
			ObjectType: entry.ObjectObjectType,
		},
		Target: &event_feed.Target{
			ID:         entry.TargetID,
			Name:       entry.TargetName,
			ObjectType: entry.TargetObjectType,
		},
	}, nil
}

func fromInternalFormatSummary(s *feed.FeedSummary) *event_feed.FeedSummaryResponse {
	totalEntries := int64(0)
	counts := make([]*event_feed.EntryCount, 0, len(s.Counts))
	for k, v := range s.Counts {
		ec := event_feed.EntryCount{Category: k, Count: v}
		counts = append(counts, &ec)
		totalEntries = totalEntries + v
	}
	return &event_feed.FeedSummaryResponse{TotalEntries: totalEntries, EntryCounts: counts}
}

func fromInternalFormatActionLine(line *feed.ActionLine) *event_feed.ActionLine {
	// for the given action line, convert counts data in each time slot to external (gRPC) format
	timeslots := make([]*event_feed.Timeslot, len(line.Timeslots))
	for i, slot := range line.Timeslots {
		timeslots[i] = &event_feed.Timeslot{
			Counts: make([]*event_feed.EntryCount, len(slot.EntryCounts)),
		}
		for j, count := range slot.EntryCounts {
			countData := event_feed.EntryCount{Category: count.Category, Count: count.Count}
			timeslots[i].Counts[j] = &countData
		}
	}
	return &event_feed.ActionLine{Action: line.Action, Slots: timeslots}
}
