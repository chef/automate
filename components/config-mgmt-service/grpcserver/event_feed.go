package grpcserver

import (
	"context"
	"time"

	"github.com/golang/protobuf/ptypes"
	log "github.com/sirupsen/logrus"

	"github.com/chef/automate/api/interservice/cfgmgmt/request"
	"github.com/chef/automate/api/interservice/cfgmgmt/response"
	"github.com/chef/automate/components/config-mgmt-service/backend"
	"github.com/chef/automate/components/config-mgmt-service/errors"
	"github.com/chef/automate/components/config-mgmt-service/params"

	"google.golang.org/grpc/codes"
)

// GetEventFeed returns a list of all Events
func (s *CfgMgmtServer) GetEventFeed(ctx context.Context,
	request *request.EventFilter) (*response.Events, error) {
	eventCollection := &response.Events{}

	log.WithFields(log.Fields{
		"request": request.String(),
		"func":    nameOfFunc(),
	}).Debug("rpc call")

	filters, err := params.FormatActionFilters(request.Filter)
	if err != nil {
		return eventCollection, errors.GrpcErrorFromErr(codes.InvalidArgument, err)
	}

	filters, err = filterByProjects(ctx, filters)
	if err != nil {
		return eventCollection, errors.GrpcErrorFromErr(codes.Internal, err)
	}

	// Date Range
	startTime, endTime, err := params.ValidateMillsecondDateRange(request.Start, request.End)
	if err != nil {
		return eventCollection, errors.GrpcErrorFromErr(codes.InvalidArgument, err)
	}

	// Paging Parameters
	cursorTime, ascending, err := params.ValidatePagingCursorTime(request.Before, request.After,
		request.Cursor, request.End)
	if err != nil {
		return eventCollection, errors.GrpcErrorFromErr(codes.InvalidArgument, err)
	}

	if request.PageSize <= 0 {
		return eventCollection, errors.GrpcError(codes.InvalidArgument,
			"The page size must be greater than 0")
	}

	actions, totalEvents, err := s.client.GetActions(
		filters,
		startTime,
		endTime,
		int(request.PageSize),
		cursorTime,
		request.Cursor,
		ascending,
	)

	if err != nil {
		return eventCollection, errors.GrpcErrorFromErr(codes.Internal, err)
	}

	events, err := actionsToEvents(actions)
	if err != nil {
		return eventCollection, errors.GrpcErrorFromErr(codes.Internal, err)
	}

	return &response.Events{
		Events:      events,
		TotalEvents: totalEvents,
	}, nil
}

// GetEventTaskCounts - gets event task counts
func (s *CfgMgmtServer) GetEventTaskCounts(ctx context.Context,
	request *request.EventCountsFilter) (*response.EventCounts, error) {
	log.WithFields(log.Fields{
		"request": request.String(),
		"func":    nameOfFunc(),
	}).Debug("rpc call")

	filters, err := params.FormatActionFilters(request.Filter)
	if err != nil {
		return &response.EventCounts{}, errors.GrpcErrorFromErr(codes.InvalidArgument, err)
	}

	// Date Range
	startTime, endTime, err := params.ValidateMillsecondDateRange(request.Start, request.End)
	if err != nil {
		return &response.EventCounts{}, errors.GrpcErrorFromErr(codes.InvalidArgument, err)
	}

	eventTaskCounts, err := s.client.GetActionEventTaskCounts(
		filters,
		startTime,
		endTime,
	)

	if err != nil {
		return &response.EventCounts{}, errors.GrpcErrorFromErr(codes.Internal, err)
	}

	return toEventCounts(eventTaskCounts), nil
}

// GetEventTypeCounts - gets event type counts
func (s *CfgMgmtServer) GetEventTypeCounts(ctx context.Context,
	request *request.EventCountsFilter) (*response.EventCounts, error) {
	log.WithFields(log.Fields{
		"request": request.String(),
		"func":    nameOfFunc(),
	}).Debug("rpc call")

	filters, err := params.FormatActionFilters(request.Filter)
	if err != nil {
		return &response.EventCounts{}, errors.GrpcErrorFromErr(codes.InvalidArgument, err)
	}

	// Date Range
	startTime, endTime, err := params.ValidateMillsecondDateRange(request.Start, request.End)
	if err != nil {
		return &response.EventCounts{}, errors.GrpcErrorFromErr(codes.InvalidArgument, err)
	}

	eventTypeCounts, err := s.client.GetActionEventTypeCounts(
		filters,
		startTime,
		endTime,
	)

	if err != nil {
		return &response.EventCounts{}, errors.GrpcErrorFromErr(codes.Internal, err)
	}

	return toEventCounts(eventTypeCounts), nil
}

// GetEventStringBuckets - gets the buckets for the guitar strings
// create = 2 update = 1 delete = 0
// ----------------------------------------------------------------------------
// |         Jan 28         |         Jan 29         |         Jan 30         |
// |_____________b__________|________________________|_____________i__________|'create'
// |________________________|_____________c__________|________________________|'update'
// |________________________|________________________|________________________|'delete'
// ----------------------------------------------------------------------------
// 24 hours in a day is enforced. For daylight savings time an hour is added or moved.
func (s *CfgMgmtServer) GetEventStringBuckets(ctx context.Context, request *request.EventStrings) (*response.EventStrings, error) {
	log.WithFields(log.Fields{
		"request": request.String(),
		"func":    nameOfFunc(),
	}).Debug("rpc call")

	var (
		tasksMap              = []string{"create", "update", "delete"}
		eventStringCollection = make([]*response.EventString, len(tasksMap))
	)

	err := s.validateEventStringsRequest(request)
	if err != nil {
		return &response.EventStrings{}, errors.GrpcErrorFromErr(codes.InvalidArgument, err)
	}

	filters, err := params.FormatActionFilters(request.Filter)
	if err != nil {
		return &response.EventStrings{}, errors.GrpcErrorFromErr(codes.InvalidArgument, err)
	}

	filters, err = filterByProjects(ctx, filters)
	if err != nil {
		return &response.EventStrings{}, errors.GrpcErrorFromErr(codes.Internal, err)
	}

	// Create the three strings
	for i, task := range tasksMap {
		eventString, err := s.client.GetEventString(
			filters, request.Start, request.End,
			request.Timezone, int(request.HoursBetween), task,
		)
		if err != nil {
			return &response.EventStrings{}, errors.GrpcErrorFromErr(codes.Internal, err)
		}

		eventsCollection := make([]*response.EventCollection, len(eventString.EventsCollection))
		for e, eCollection := range eventString.EventsCollection {
			eventsCollection[e] = &response.EventCollection{
				EventsCount: make([]*response.EventCount, len(eCollection.EventsCount)),
			}
			for c, eCount := range eCollection.EventsCount {
				castEventCount := response.EventCount{Name: eCount.Name, Count: eCount.Count}
				eventsCollection[e].EventsCount[c] = &castEventCount
			}
		}

		eventStringCollection[i] = &response.EventString{
			Collection:  eventsCollection,
			EventAction: eventString.EventAction,
		}
	}

	return &response.EventStrings{
		Start:        request.Start,
		End:          request.End,
		HoursBetween: request.HoursBetween,
		Strings:      eventStringCollection,
	}, nil
}

func (s *CfgMgmtServer) validateEventStringsRequest(request *request.EventStrings) error {
	// Validate TimeZone
	if request.Timezone == "" {
		return errors.GrpcError(codes.InvalidArgument, "A timezone must be provided")
	}

	_, err := time.LoadLocation(request.Timezone)
	if err != nil {
		return errors.GrpcErrorFromErr(codes.InvalidArgument, err)
	}

	// Validate HoursBetween
	if request.HoursBetween <= 0 || request.HoursBetween > 24 {
		return errors.GrpcError(codes.InvalidArgument,
			"HoursBetween must be greater than 0 and less than or equal to 24")
	}

	if 24%request.HoursBetween != 0 {
		return errors.GrpcError(codes.InvalidArgument, "24 must be divisible by HoursBetween")
	}

	// Validate Date Range
	if !params.ValidateDateRange(request.Start, request.End) {
		return errors.GrpcError(codes.InvalidArgument, "Invalid start/end time. (format: YYYY-MM-DD)")
	}

	return nil
}

func toEventCounts(eventTypesCounts map[string]int64) *response.EventCounts {
	var (
		eventCounts = make([]*response.EventCount, len(eventTypesCounts))
		total       int64
		index       = 0
	)

	for key, value := range eventTypesCounts {
		eventCounts[index] = &response.EventCount{
			Name:  key,
			Count: value,
		}
		total = total + value
		index++
	}

	return &response.EventCounts{
		Total:  total,
		Counts: eventCounts,
	}
}

func actionsToEvents(actions []backend.Action) ([]*response.Event, error) {
	csEvents := make([]*response.Event, len(actions))

	for index, action := range actions {
		timestamp, err := ptypes.TimestampProto(action.RecordedAt)
		if err != nil {
			log.WithFields(log.Fields{
				"func":      nameOfFunc(),
				"err":       err,
				"timestamp": action.RecordedAt.String(),
			}).Warn("Unable to translate checkin time to timestamp proto")
			return csEvents, err
		}
		csEvents[index] = &response.Event{
			Id:              action.Id,
			EventType:       action.EntityType,
			Task:            action.Task,
			Timestamp:       timestamp,
			EntityName:      action.EntityName,
			RequestorType:   action.RequestorType,
			RequestorName:   action.RequestorName,
			ServiceHostname: action.ServiceHostname,
			ParentName:      action.ParentName,
			ParentType:      action.ParentType,
		}
	}
	return csEvents, nil
}
