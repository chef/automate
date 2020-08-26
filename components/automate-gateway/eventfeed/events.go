package eventfeed

import (
	"context"
	"sort"
	"strings"
	"time"

	agReq "github.com/chef/automate/api/external/event_feed/request"
	agRes "github.com/chef/automate/api/external/event_feed/response"
	event_feed_api "github.com/chef/automate/api/interservice/event_feed"
	"github.com/golang/protobuf/ptypes"
	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/status"
)

type EventFeedAggregate struct {
	feedServiceClient event_feed_api.EventFeedServiceClient
}

func NewEventFeedAggregate(feedClient event_feed_api.EventFeedServiceClient) *EventFeedAggregate {
	return &EventFeedAggregate{
		feedServiceClient: feedClient,
	}
}

// CollectEventFeed - collection the events from all the components
//
// It is important to mention that we will be able to survive if one of the
// components return an error but the rest of them don't, the reason why is because
// the gateway is just collecting information from the downstream services and it
// should be able to handle errors.
func (eventFeedAggregate *EventFeedAggregate) CollectEventFeed(ctx context.Context,
	request *agReq.GetEventFeedRequest) (*agRes.GetEventFeedResponse, error) {

	var (
		ascending                 = false
		isLastPage                = false
		totalEvents               = make([]*agRes.Event, 0)
		totalNumberOfEvents int64 = 0
		err                       = validateRequest(request)
	)

	if err != nil {
		return &agRes.GetEventFeedResponse{}, err
	}

	// This case is if the request is for the last page. For the last page request
	// the request.After must be equal to the request.End and the request.Cursor should not be set
	if request.After > 0 && request.Cursor == "" && request.After == request.End { // last page
		ascending = true
		isLastPage = true
		// This is the case for the previous page. Here the request.After and request.Cursor must be set.
	} else if request.After > 0 && request.Cursor != "" { // previous page
		ascending = true
	}

	eventCollection, err := collectEventFeed(ctx, eventFeedAggregate.feedServiceClient, request)
	if err != nil {
		return &agRes.GetEventFeedResponse{}, err
	}
	totalEvents = eventCollection.Events
	totalNumberOfEvents += eventCollection.TotalEvents

	err = sortEvents(totalEvents, ascending)
	if err != nil {
		return &agRes.GetEventFeedResponse{}, err
	}

	// If this is not the last page the max number of events to take is the page size.
	numberOfEventsToTake := int(request.PageSize)

	// When the last page is requested we only want to return the remaining events
	// For example if all the events for the entire request.Start to request.End range is 45
	// then the last page should only return 5 events.
	if isLastPage {
		numberOfEventsToTake = int(totalNumberOfEvents) % int(request.PageSize)

		if numberOfEventsToTake == 0 {
			numberOfEventsToTake = int(request.PageSize)
		}
	}

	// Take only the page size amount of events and drop the other events.
	// If the total number of events is less than the page size, take all the events.
	if numberOfEventsToTake < len(totalEvents) {
		totalEvents = totalEvents[:numberOfEventsToTake]
	}

	if ascending {
		reverseEvents(totalEvents)
	}

	if request.GetCollapse() {
		totalEvents = groupEvents(totalEvents)
	}

	return &agRes.GetEventFeedResponse{
		Events:      totalEvents,
		TotalEvents: totalNumberOfEvents,
	}, nil
}

type eventWithTime struct {
	event     *agRes.Event
	timestamp time.Time
}

func sortEvents(eventCollection []*agRes.Event, ascending bool) error {
	eventTimeCollection := make([]eventWithTime, len(eventCollection))

	// This is needed to be able to catch the error from converting the TimeStamp to a time.Time object
	for index, event := range eventCollection {
		date, err := ptypes.Timestamp(event.StartTime)
		if err != nil {
			return err
		}
		eventTimeCollection[index] = eventWithTime{event: event, timestamp: date}
	}

	// sort by Timestamp
	sort.Slice(eventTimeCollection, func(i, j int) bool {
		iDate := eventTimeCollection[i].timestamp
		jDate := eventTimeCollection[j].timestamp

		// If dates are equal sort on IDs
		if iDate.Equal(jDate) {
			iID := eventTimeCollection[i].event.StartId
			jID := eventTimeCollection[j].event.StartId
			if ascending {
				return strings.Compare(iID, jID) < 0
			}
			return strings.Compare(iID, jID) > 0
		}

		if ascending {
			return iDate.Before(jDate)
		}
		return iDate.After(jDate)
	})

	for index, eventTime := range eventTimeCollection {
		eventCollection[index] = eventTime.event
	}

	return nil
}

func validateRequest(request *agReq.GetEventFeedRequest) error {
	if request.Start != 0 && request.End != 0 && request.Start > request.End {
		return status.Error(codes.InvalidArgument, "Invalid start/end time. End before Start")
	}

	// Paging Parameters
	if request.Before > 0 && request.After > 0 {
		return status.Error(codes.InvalidArgument,
			"Invalid 'before'/'after' param. Both parameters should not be set")
	}

	if request.Before > 0 && request.Cursor == "" {
		return status.Error(codes.InvalidArgument,
			"Invalid 'before' param. If the 'before' parameter is set the 'cursor' must be set also")
	}

	if request.After > 0 && request.Cursor == "" && request.After != request.End {
		return status.Error(codes.InvalidArgument,
			"Invalid 'after' param. If the 'after' parameter is set and not the 'cursor', then the 'after' must be equal to the 'end'")
	}

	// Page size
	if request.PageSize <= 0 {
		return status.Error(codes.InvalidArgument,
			"The page size must be greater than 0")
	}

	return nil
}

func reverseEvents(events []*agRes.Event) {
	length := len(events)
	for i := length/2 - 1; i >= 0; i-- {
		opp := length - 1 - i
		events[i], events[opp] = events[opp], events[i]
	}
}

func groupEvents(events []*agRes.Event) []*agRes.Event {
	groupedEvents := make([]*agRes.Event, 0)
	index := 0
	for _, event := range events {
		if index > 0 {
			if groupedEvents[index-1].EventType == event.GetEventType() &&
				groupedEvents[index-1].Task == event.GetTask() &&
				groupedEvents[index-1].RequestorName == event.GetRequestorName() {
				// if the type, action and user are the same, then increment the
				// event count +1 and continue processing the next event
				groupedEvents[index-1].EventCount++
				groupedEvents[index-1].StartTime = event.GetEndTime()
				groupedEvents[index-1].StartId = event.GetEndId()
				continue
			}
		}

		groupedEvents = append(groupedEvents, &agRes.Event{
			StartId:          event.GetStartId(),
			EndId:            event.GetEndId(),
			EventType:        event.GetEventType(),
			Task:             event.GetTask(),
			StartTime:        event.GetStartTime(),
			EndTime:          event.GetEndTime(),
			EntityName:       event.GetEntityName(),
			RequestorType:    event.GetRequestorType(),
			RequestorName:    event.GetRequestorName(),
			ServiceHostname:  event.GetServiceHostname(),
			ParentName:       event.GetParentName(),
			ParentType:       event.GetParentType(),
			ChefInfraServer:  event.ChefInfraServer,
			ChefOrganization: event.ChefOrganization,
			EventCount:       1,
		})

		index++
	}

	return groupedEvents
}

func collectEventFeed(ctx context.Context,
	feedClient event_feed_api.EventFeedServiceClient,
	request *agReq.GetEventFeedRequest) (*agRes.GetEventFeedResponse, error) {
	eventFilter := &event_feed_api.FeedRequest{
		Filters: request.GetFilter(),
		Start:   request.GetStart(),
		End:     request.GetEnd(),
		Size:    request.GetPageSize(),
		After:   request.GetAfter(),
		Before:  request.GetBefore(),
		Cursor:  request.GetCursor(),
	}

	eventCollection, err := feedClient.GetFeed(ctx, eventFilter)
	if err != nil {
		return &agRes.GetEventFeedResponse{}, err
	}

	agEvents := make([]*agRes.Event, len(eventCollection.FeedEntries))
	for index, entry := range eventCollection.FeedEntries {
		agEvents[index] = &agRes.Event{
			StartId:          entry.GetId(),
			EndId:            entry.GetId(),
			EventType:        entry.GetProducer().GetId(),
			Task:             entry.GetVerb(),
			StartTime:        entry.GetSourceEventPublished(),
			EndTime:          entry.GetSourceEventPublished(),
			EntityName:       entry.GetObject().GetName(),
			RequestorType:    entry.GetActor().GetObjectType(),
			RequestorName:    entry.GetActor().GetName(),
			ServiceHostname:  entry.GetTarget().GetName(),
			ParentName:       entry.GetParent().GetName(),
			ParentType:       entry.GetParent().GetId(),
			EventCount:       1,
			ChefInfraServer:  entry.ChefInfraServer,
			ChefOrganization: entry.ChefOrganization,
		}
	}

	return &agRes.GetEventFeedResponse{
		Events:      agEvents,
		TotalEvents: eventCollection.TotalEntries,
	}, nil
}
