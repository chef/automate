package server

import (
	"time"

	"github.com/gofrs/uuid"
	"github.com/golang/protobuf/ptypes"
	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"
	"google.golang.org/grpc/codes"

	api "github.com/chef/automate/api/interservice/event"
	"github.com/chef/automate/api/interservice/event_feed"
	e "github.com/chef/automate/components/event-feed-service/pkg/errors"
	"github.com/chef/automate/components/event-feed-service/pkg/feed"
	"github.com/chef/automate/components/event-feed-service/pkg/persistence"
	"github.com/chef/automate/lib/stringutils"
)

type FeedService struct {
	store persistence.FeedStore
}

func NewFeedService(feedStore persistence.FeedStore) *FeedService {
	return &FeedService{store: feedStore}
}

func (f *FeedService) GetFeed(req *event_feed.FeedRequest) ([]*feed.FeedEntry, int64, error) {

	// Date Range
	startTime, endTime, err := feed.ValidateMillisecondDateRange(req.Start, req.End)
	if err != nil {
		return nil, 0, e.GrpcErrorFromErr(codes.InvalidArgument, err)
	}

	// Paging Parameters
	cursorTime, ascending, err := feed.ValidatePagingCursorTime(req.Before, req.After, req.Cursor, req.End)
	if err != nil {
		return nil, 0, e.GrpcErrorFromErr(codes.InvalidArgument, err)
	}

	if req.Size <= 0 {
		return nil, 0, e.GrpcError(codes.InvalidArgument,
			"The page size must be greater than 0")
	}

	filters, err := feed.FormatFilters(req.Filters)
	if err != nil {
		return nil, 0, err
	}

	fq := feed.FeedQuery{
		UserID:     req.UserID,
		Size:       int(req.Size),
		Start:      startTime.UTC(),
		End:        endTime.UTC(),
		Filters:    filters,
		CursorDate: cursorTime.UTC(),
		CursorID:   req.Cursor,
		Ascending:  ascending,
	}

	resp, hits, err := f.store.GetFeed(&fq)
	if err != nil {
		return nil, 0, e.GrpcErrorFromErr(codes.Internal, err)
	}
	return resp, hits, nil
}

func (f *FeedService) GetFeedSummary(countCategory string, filters []string,
	start time.Time, end time.Time) (*feed.FeedSummary, error) {

	// remove count category from the filters array
	filters = stringutils.SliceReject(filters, countCategory)

	mapFilters, err := feed.FormatFilters(filters)
	if err != nil {
		return nil, err
	}

	fq := feed.FeedSummaryQuery{
		CountsCategory: feed.ConvertAPIKeyToBackendKey(countCategory),
		Buckets:        false,
		Start:          start,
		End:            end,
		Filters:        mapFilters,
	}
	counts, err := f.store.GetFeedSummary(&fq)
	if err != nil {
		return nil, errors.Wrap(err, "failed to get feed summary from persistence store")
	}

	return &feed.FeedSummary{Counts: counts}, nil
}

// interval = time span for which to collect activity entries in hours
func (f *FeedService) GetActionLine(filters []string, startDate string, endDate string, timezone string, interval int, action string) (*feed.ActionLine, error) {

	// remove action (aka "task") from the filters array...
	// we're already filtering lines by action
	filters = stringutils.SliceReject(filters, action)
	line, err := f.store.GetActionLine(filters, startDate, endDate, timezone, interval, action)
	if err != nil {
		return &feed.ActionLine{}, errors.Wrap(err, "failed to get timeline from persistence store")
	}

	return line, nil
}

// Going forward, we'll need to maintain a map of event types to feed type(s). Since the
// only events we support currently map to the "event" feed type, we're hard-coding the feed type below.
func (f *FeedService) HandleEvent(req *api.EventMsg) (*api.EventResponse, error) {
	logrus.Debug("automate-feed is handling your event...")

	publishedAt, err := ptypes.Timestamp(req.Published)
	if err != nil {
		return nil, e.GrpcErrorFromErr(codes.InvalidArgument, err)
	}

	// translate event message into feed entry
	feedEntry := feed.FeedEntry{
		ID:                 uuid.Must(uuid.NewV4()).String(),
		ProducerID:         req.Producer.ID,
		ProducerName:       req.Producer.ProducerName,
		ProducerObjectType: req.Producer.ProducerType,
		ProducerTags:       req.Producer.Tags,
		FeedType:           "event",
		EventType:          req.Type.Name,
		Tags:               req.Tags,
		Published:          publishedAt.UTC(),
		ActorID:            req.Actor.ID,
		ActorName:          req.Actor.DisplayName,
		ActorObjectType:    req.Actor.ObjectType,
		Verb:               req.Verb,
		ObjectID:           req.Object.ID,
		ObjectName:         req.Object.DisplayName,
		ObjectObjectType:   req.Object.ObjectType,
		TargetID:           req.Target.ID,
		TargetName:         req.Target.DisplayName,
		TargetObjectType:   req.Target.ObjectType,
		Created:            time.Now().UTC(),
	}

	success, err := f.store.CreateFeedEntry(&feedEntry)
	if err != nil {
		return nil, errors.Wrap(err, "failed to create feed entry in persistence store")
	}

	return &api.EventResponse{Success: success}, nil
}
