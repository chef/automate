package eventfeed

import (
	"context"
	"fmt"
	"sort"
	"strings"
	"sync"
	"time"

	cmsReq "github.com/chef/automate/api/interservice/cfgmgmt/request"
	cmsService "github.com/chef/automate/api/interservice/cfgmgmt/service"
	event_feed_api "github.com/chef/automate/api/interservice/event_feed"
	agReq "github.com/chef/automate/components/automate-gateway/api/event_feed/request"
	agRes "github.com/chef/automate/components/automate-gateway/api/event_feed/response"
	"github.com/golang/protobuf/ptypes"
	log "github.com/sirupsen/logrus"
	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/status"
)

// A eventsFunc type returns a subset of events and an error
type eventsFunc func() (*agRes.Events, error)

type EventFeedAggregate struct {
	cfgMgmtClient     cmsService.CfgMgmtClient
	feedServiceClient event_feed_api.EventFeedServiceClient
}

func NewEventFeedAggregate(cfgMgmtClient cmsService.CfgMgmtClient,
	feedClient event_feed_api.EventFeedServiceClient) *EventFeedAggregate {
	return &EventFeedAggregate{
		cfgMgmtClient:     cfgMgmtClient,
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
	request *agReq.EventFilter) (*agRes.Events, error) {

	var (
		ascending                   = false
		isLastPage                  = false
		totalErrors                 = make([]string, 0)
		subscribers                 = 2
		totalEvents                 = make([]*agRes.Event, 0)
		totalNumberOfEvents   int64 = 0
		err                         = validateRequest(request)
		collectComplianceFunc       = func() (*agRes.Events, error) {
			return collectComplianceEventFeed(ctx, eventFeedAggregate.feedServiceClient, request)
		}
		collectCfgmgmtFunc = func() (*agRes.Events, error) {
			return collectConfigMgmtEventFeed(ctx, eventFeedAggregate.cfgMgmtClient, request)
		}
	)

	if err != nil {
		return &agRes.Events{}, err
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

	c1, e1 := fanOutEvents("config-mgmt-service", collectCfgmgmtFunc)
	c2, e2 := fanOutEvents("compliance-service", collectComplianceFunc)

	for e := range fanErrIn(e1, e2) {
		if e != nil {
			totalErrors = append(totalErrors, e.Error())
		}
	}

	// If the number of errors is major or equal to the number of subscribers
	// it means that we don't have any data to display, lets return an error
	if len(totalErrors) >= subscribers {
		return &agRes.Events{}, fmt.Errorf(strings.Join(totalErrors, "\n"))
	}

	// TODO @afiune - In the near future we would like our API to handle partial
	// errors so that we don't ignore them here, but for now we will.

	for c := range fanInEvents(c1, c2) {
		totalEvents = append(totalEvents, c.Events...)
		totalNumberOfEvents += c.TotalEvents
	}

	err = sortEvents(totalEvents, ascending)
	if err != nil {
		return &agRes.Events{}, err
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

	return &agRes.Events{
		Events:      totalEvents,
		TotalEvents: totalNumberOfEvents,
	}, nil
}

func fanOutEvents(component string, fn eventsFunc) (<-chan *agRes.Events, <-chan error) {
	errorChan := make(chan error)
	eventsChan := make(chan *agRes.Events)
	go func() {
		events, err := fn()
		if err != nil {
			log.WithFields(log.Fields{
				"err":       err,
				"component": component,
			}).Error("collecting events")
		}
		errorChan <- err
		close(errorChan)
		eventsChan <- events
		close(eventsChan)
	}()
	return eventsChan, errorChan
}

func fanInEvents(cs ...<-chan *agRes.Events) <-chan *agRes.Events {
	var wg sync.WaitGroup
	out := make(chan *agRes.Events)

	output := func(c <-chan *agRes.Events) {
		for n := range c {
			out <- n
		}
		wg.Done()
	}
	wg.Add(len(cs))
	for _, c := range cs {
		go output(c)
	}

	go func() {
		wg.Wait()
		close(out)
	}()
	return out
}

func fanErrIn(cs ...<-chan error) <-chan error {
	var wg sync.WaitGroup
	out := make(chan error)

	output := func(c <-chan error) {
		for n := range c {
			out <- n
		}
		wg.Done()
	}
	wg.Add(len(cs))
	for _, c := range cs {
		go output(c)
	}

	go func() {
		wg.Wait()
		close(out)
	}()
	return out
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

func validateRequest(request *agReq.EventFilter) error {
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
			StartId:         event.GetStartId(),
			EndId:           event.GetEndId(),
			EventType:       event.GetEventType(),
			Task:            event.GetTask(),
			StartTime:       event.GetStartTime(),
			EndTime:         event.GetEndTime(),
			EntityName:      event.GetEntityName(),
			RequestorType:   event.GetRequestorType(),
			RequestorName:   event.GetRequestorName(),
			ServiceHostname: event.GetServiceHostname(),
			ParentName:      event.GetParentName(),
			ParentType:      event.GetParentType(),
			EventCount:      1,
		})

		index++
	}

	return groupedEvents
}

func collectConfigMgmtEventFeed(ctx context.Context,
	cfgMgmtClient cmsService.CfgMgmtClient,
	request *agReq.EventFilter) (*agRes.Events, error) {
	eventFilter := &cmsReq.EventFilter{
		Filter:   request.GetFilter(),
		Start:    request.GetStart(),
		End:      request.GetEnd(),
		PageSize: request.GetPageSize(),
		After:    request.GetAfter(),
		Before:   request.GetBefore(),
		Cursor:   request.GetCursor(),
	}

	csEventCollection, err := cfgMgmtClient.GetEventFeed(ctx, eventFilter)
	if err != nil {
		return &agRes.Events{}, err
	}

	agEvents := make([]*agRes.Event, len(csEventCollection.Events))
	for index, csEvent := range csEventCollection.Events {
		agEvents[index] = &agRes.Event{
			StartId:         csEvent.GetId(),
			EndId:           csEvent.GetId(),
			EventType:       csEvent.GetEventType(),
			Task:            csEvent.GetTask(),
			StartTime:       csEvent.GetTimestamp(),
			EndTime:         csEvent.GetTimestamp(),
			EntityName:      csEvent.GetEntityName(),
			RequestorType:   csEvent.GetRequestorType(),
			RequestorName:   csEvent.GetRequestorName(),
			ServiceHostname: csEvent.GetServiceHostname(),
			ParentName:      csEvent.GetParentName(),
			ParentType:      csEvent.GetParentType(),
			EventCount:      1,
		}
	}

	return &agRes.Events{
		Events:      agEvents,
		TotalEvents: csEventCollection.TotalEvents,
	}, nil
}

func collectComplianceEventFeed(ctx context.Context,
	feedClient event_feed_api.EventFeedServiceClient,
	request *agReq.EventFilter) (*agRes.Events, error) {
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
		return &agRes.Events{}, err
	}

	agEvents := make([]*agRes.Event, len(eventCollection.FeedEntries))
	for index, complianceEvent := range eventCollection.FeedEntries {
		agEvents[index] = &agRes.Event{
			StartId:         complianceEvent.GetID(),
			EndId:           complianceEvent.GetID(),
			EventType:       complianceEvent.GetProducer().GetID(),
			Task:            complianceEvent.GetVerb(),
			StartTime:       complianceEvent.GetSourceEventPublished(),
			EndTime:         complianceEvent.GetSourceEventPublished(),
			EntityName:      complianceEvent.GetObject().GetName(),
			RequestorType:   complianceEvent.GetActor().GetObjectType(),
			RequestorName:   complianceEvent.GetActor().GetName(),
			ServiceHostname: complianceEvent.GetTarget().GetName(),
			ParentName:      "Not Applicable",
			ParentType:      "Not Applicable",
			EventCount:      1,
		}
	}

	return &agRes.Events{
		Events:      agEvents,
		TotalEvents: eventCollection.TotalEntries,
	}, nil
}
