//
//  Author:: Gina Peers <gpeers@chef.io>, Salim Afiune <afiune@chef.io>
//  Copyright:: Copyright 2018, Chef Software Inc.
//

package feed

import (
	"time"

	"github.com/gofrs/uuid"
	"github.com/golang/protobuf/ptypes"
	"github.com/sirupsen/logrus"
	"google.golang.org/grpc/codes"

	api "github.com/chef/automate/api/interservice/event"
	automate_feed "github.com/chef/automate/components/compliance-service/api/automate-feed"
	"github.com/chef/automate/components/compliance-service/feed/errors"
	"github.com/chef/automate/components/compliance-service/feed/persistence"
	"github.com/chef/automate/components/compliance-service/feed/util"
	"github.com/chef/automate/components/compliance-service/reporting/relaxting"
)

type Feeds struct {
	store persistence.FeedStore
}

func NewFeeds(conn *relaxting.ES2Backend) *Feeds {
	store := persistence.NewFeedStore(conn)
	return &Feeds{store: store}
}

func (f *Feeds) GetFeed(req *automate_feed.FeedRequest) ([]*util.FeedEntry, int64, error) {
	// Date Range
	startTime, endTime, err := util.ValidateMillisecondDateRange(req.Start, req.End)
	if err != nil {
		return nil, 0, errors.GrpcErrorFromErr(codes.InvalidArgument, err)
	}

	// Paging Parameters
	cursorTime, ascending, err := util.ValidatePagingCursorTime(req.Before, req.After, req.Cursor, req.End)
	if err != nil {
		return nil, 0, errors.GrpcErrorFromErr(codes.InvalidArgument, err)
	}

	if req.Size <= 0 {
		return nil, 0, errors.GrpcError(codes.InvalidArgument,
			"The page size must be greater than 0")
	}

	fq := util.FeedQuery{
		UserID:     req.UserID,
		Size:       int(req.Size),
		Start:      startTime.UTC(),
		End:        endTime.UTC(),
		Filters:    req.Filters,
		CursorDate: cursorTime.UTC(),
		CursorID:   req.Cursor,
		Ascending:  ascending,
	}

	resp, hits, err := f.store.GetFeed(&fq)
	if err != nil {
		return nil, 0, errors.GrpcErrorFromErr(codes.Internal, err)
	}
	return resp, hits, nil
}

func (f *Feeds) GetFeedSummary(countCategory string, filters []string, start time.Time, end time.Time) (*util.FeedSummary, error) {

	// remove count category from the filters array
	filters = util.Remove(filters, countCategory)

	fq := util.FeedSummaryQuery{
		CountsCategory: countCategory,
		Buckets:        false,
		Start:          start,
		End:            end,
		Filters:        filters,
	}
	counts, err := f.store.GetFeedSummary(&fq)
	if err != nil {
		logrus.Warnf("Could not get feed summary; error: %+v", err)
		return nil, err
	}

	return &util.FeedSummary{Counts: counts}, nil
}

// interval = time span for which to collect activity entries in hours
func (f *Feeds) GetActionLine(filters []string, startDate string, endDate string, timezone string, interval int, action string) (*util.ActionLine, error) {

	// remove action (aka "task") from the filters array...
	// we're already filtering lines by action
	filters = util.Remove(filters, action)
	line, err := f.store.GetActionLine(filters, startDate, endDate, timezone, interval, action)
	if err != nil {
		logrus.Warnf("Could not get feed timeline; error: %+v", err)
		return &util.ActionLine{}, err
	}

	return line, nil
}

// Going forward, we'll need to maintain a map of event types to feed type(s). Since the
// only events we support currently map to the "event" feed type, we're hard-coding the feed type below.
func (f *Feeds) HandleEvent(req *api.EventMsg) (*api.EventResponse, error) {
	logrus.Debug("automate-feed is handling your event...")

	publishedAt, err := ptypes.Timestamp(req.Published)
	if err != nil {
		return nil, errors.GrpcErrorFromErr(codes.InvalidArgument, err)
	}

	// translate event message into feed entry
	feedEntry := util.FeedEntry{
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
	logrus.Debugf("Event was successfully handled: %t", success)
	res := api.EventResponse{Success: success}

	if err != nil {
		logrus.Warn("Event was not handled... error creating feed entry")
		return &res, err
	}

	logrus.Debug("automate-feed has finished handling your event...")

	return &res, nil
}
