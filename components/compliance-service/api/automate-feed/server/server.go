//
//  Author:: Gina Peers <gpeers@chef.io>, Salim Afiune <afiune@chef.io>
//  Copyright:: Copyright 2018, Chef Software Inc.
//
package server

import (
	"context"
	"time"

	"github.com/golang/protobuf/ptypes"
	"github.com/sirupsen/logrus"
	"google.golang.org/grpc/codes"

	automate_event "github.com/chef/automate/api/interservice/event"
	automate_feed "github.com/chef/automate/components/compliance-service/api/automate-feed"
	"github.com/chef/automate/components/compliance-service/config"
	"github.com/chef/automate/components/compliance-service/feed"
	"github.com/chef/automate/components/compliance-service/feed/errors"
	"github.com/chef/automate/components/compliance-service/feed/util"
	"github.com/chef/automate/components/compliance-service/reporting/relaxting"
)

type Server struct {
	feedService *feed.Feeds
}

// New creates and starts the automate-feed gRPC server
func New(config *config.FeedConfig, conn *relaxting.ES2Backend) *Server {
	// address, port, log level, etc. set on compliance service startup;
	// should be set here when automate-feed becomes an independent service

	// new grpcServer FeedService spun up on compliance service startup;
	// should be spun up here when automate-feed becomes an independent service

	// new server
	server := Server{
		feedService: feed.NewFeeds(conn)}
	return &server
}

// TODO: @gcp refactor so that param validations & resulting grpc errors thrown here
// TODO: @gcp consistent behavior on err w/r/t returning either empty result set or nil
func (s *Server) GetFeed(ctx context.Context, req *automate_feed.FeedRequest) (*automate_feed.FeedResponse, error) {
	var feedEntries []*util.FeedEntry
	var hits int64
	var err error

	logrus.WithFields(logrus.Fields{
		"request": req.String(),
		"func":    util.NameOfFunc(),
	}).Info("rpc call")

	if feedEntries, hits, err = s.feedService.GetFeed(req); err != nil {
		logrus.Warnf("Couldn't get feed entries; error: %v", err)
		return nil, err
	}

	fe, err := FromInternalFormatToList(feedEntries)
	if err != nil {
		return nil, err
	}

	return &automate_feed.FeedResponse{
		FeedEntries:  fe,
		TotalEntries: hits,
	}, nil
}

func (s *Server) GetFeedSummary(ctx context.Context, req *automate_feed.FeedSummaryRequest) (*automate_feed.FeedSummaryResponse, error) {
	logrus.WithFields(logrus.Fields{
		"request": req.String(),
		"func":    util.NameOfFunc(),
	}).Info("rpc call")

	// Date Range
	startTime, endTime, err := util.ValidateMillisecondDateRange(req.Start, req.End)
	if err != nil {
		return &automate_feed.FeedSummaryResponse{}, errors.GrpcErrorFromErr(codes.InvalidArgument, err)
	}

	fs, err := s.feedService.GetFeedSummary(req.CountCategory, req.Filters, startTime, endTime)
	if err != nil {
		return &automate_feed.FeedSummaryResponse{}, errors.GrpcErrorFromErr(codes.Internal, err)
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
func (s *Server) GetFeedTimeline(ctx context.Context, req *automate_feed.FeedTimelineRequest) (*automate_feed.FeedTimelineResponse, error) {
	logrus.WithFields(logrus.Fields{
		"request": req.String(),
		"func":    util.NameOfFunc(),
	}).Info("rpc call")

	var (
		actionsMap  = []string{"create", "update", "delete"}
		actionLines = make([]*automate_feed.ActionLine, len(actionsMap))
	)

	err := s.validateFeedTimelineRequest(req)
	if err != nil {
		return &automate_feed.FeedTimelineResponse{}, errors.GrpcErrorFromErr(codes.InvalidArgument, err)
	}

	// Create three lines, one for each action
	for i, action := range actionsMap {
		actionLine, err := s.feedService.GetActionLine(req.Filters, req.Start, req.End, req.Timezone, int(req.Interval), action)
		if err != nil {
			return &automate_feed.FeedTimelineResponse{}, errors.GrpcErrorFromErr(codes.Internal, err)
		}
		actionLines[i] = fromInternalFormatActionLine(actionLine)
	}

	return &automate_feed.FeedTimelineResponse{
		Start:       req.Start,
		End:         req.End,
		Interval:    req.Interval,
		ActionLines: actionLines,
	}, nil
}

func (s *Server) validateFeedTimelineRequest(req *automate_feed.FeedTimelineRequest) error {
	// Validate Timezone
	if req.Timezone == "" {
		return errors.GrpcError(codes.InvalidArgument, "A timezone must be provided")
	}

	_, err := time.LoadLocation(req.Timezone)
	if err != nil {
		return errors.GrpcErrorFromErr(codes.InvalidArgument, err)
	}

	// Validate Interval
	if req.Interval <= 0 || req.Interval > 24 {
		return errors.GrpcError(codes.InvalidArgument,
			"Time interval must be greater than 0 and less than or equal to 24 hours")
	}

	if 24%req.Interval != 0 {
		return errors.GrpcError(codes.InvalidArgument, "24 must be divisible by time interval")
	}

	// Validate Date Range
	if !util.ValidateDateRange(req.Start, req.End) {
		return errors.GrpcError(codes.InvalidArgument, "Invalid start/end time. (format: YYYY-MM-DD)")
	}

	return nil
}

func fromInternalFormatActionLine(line *util.ActionLine) *automate_feed.ActionLine {
	// for the given action line, convert counts data in each time slot to external (gRPC) format
	timeslots := make([]*automate_feed.Timeslot, len(line.Timeslots))
	for i, slot := range line.Timeslots {
		timeslots[i] = &automate_feed.Timeslot{
			Counts: make([]*automate_feed.EntryCount, len(slot.EntryCounts)),
		}
		for j, count := range slot.EntryCounts {
			countData := automate_feed.EntryCount{Category: count.Category, Count: count.Count}
			timeslots[i].Counts[j] = &countData
		}
	}
	return &automate_feed.ActionLine{Action: line.Action, Slots: timeslots}
}

func fromInternalFormatSummary(s *util.FeedSummary) *automate_feed.FeedSummaryResponse {
	totalEntries := int64(0)
	var counts []*automate_feed.EntryCount
	for k, v := range s.Counts {
		ec := automate_feed.EntryCount{Category: k, Count: v}
		counts = append(counts, &ec)
		totalEntries = totalEntries + v
	}
	return &automate_feed.FeedSummaryResponse{TotalEntries: totalEntries, EntryCounts: counts}
}

func fromInternalFormat(entry *util.FeedEntry) (*automate_feed.FeedEntry, error) {
	pubTs, err := ptypes.TimestampProto(entry.Published)
	if err != nil {
		logrus.WithFields(logrus.Fields{
			"func":      util.NameOfFunc(),
			"err":       err,
			"timestamp": entry.Published.String(),
		}).Warn("Unable to translate source event publish time to timestamp proto")
		return nil, err
	}

	createTs, err := ptypes.TimestampProto(entry.Created)
	if err != nil {
		logrus.WithFields(logrus.Fields{
			"func":      util.NameOfFunc(),
			"err":       err,
			"timestamp": entry.Created.String(),
		}).Warn("Unable to translate source event publish time to timestamp proto")
		return nil, err
	}

	return &automate_feed.FeedEntry{
		ID:                   entry.ID,
		EventType:            entry.EventType,
		FeedType:             entry.FeedType,
		Tags:                 entry.Tags,
		SourceEventPublished: pubTs,
		Created:              createTs,
		Producer: &automate_feed.Producer{
			ID:         entry.ProducerID,
			Name:       entry.ProducerName,
			ObjectType: entry.ProducerObjectType,
			PTags:      entry.ProducerTags,
		},
		Actor: &automate_feed.Actor{
			ID:         entry.ActorID,
			Name:       entry.ActorName,
			ObjectType: entry.ActorObjectType,
		},
		Verb: entry.Verb,
		Object: &automate_feed.Object{
			ID:         entry.ObjectID,
			Name:       entry.ObjectName,
			ObjectType: entry.ObjectObjectType,
		},
		Target: &automate_feed.Target{
			ID:         entry.TargetID,
			Name:       entry.TargetName,
			ObjectType: entry.TargetObjectType,
		},
	}, nil
}

func FromInternalFormatToList(entries []*util.FeedEntry) ([]*automate_feed.FeedEntry, error) {
	tl := make([]*automate_feed.FeedEntry, len(entries))

	for i, entry := range entries {
		e, err := fromInternalFormat(entry)
		if err != nil {
			return nil, err
		}
		tl[i] = e
	}

	return tl, nil
}

func (s *Server) HandleEvent(ctx context.Context, req *automate_event.EventMsg) (*automate_event.EventResponse, error) {
	logrus.Debug("automate-feed server is handling your event...")
	response, err := s.feedService.HandleEvent(req)
	if err != nil {
		logrus.Warnf("Feed service couldn't handle event: %s", err)
		return response, err
	}
	return response, nil
}
