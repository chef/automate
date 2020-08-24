package eventfeed

import (
	"context"
	"math"
	"time"

	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/status"

	agReq "github.com/chef/automate/api/external/event_feed/request"
	agRes "github.com/chef/automate/api/external/event_feed/response"
	event_feed_api "github.com/chef/automate/api/interservice/event_feed"
)

// CollectEventGuitarStrings - collect the guitar strings from all the componets
//
// It is important to mention that we will be able to survive if one of the
// components return an error but the rest of them don't, the reason why is because
// the gateway is just collecting information from the downstream services and it
// should be able to handle errors.
func (eventFeedAggregate *EventFeedAggregate) CollectEventGuitarStrings(ctx context.Context,
	request *agReq.GetEventStringBucketsRequest) (*agRes.GetEventStringBucketsResponse, error) {
	err := validateEventStringsRequest(request)
	if err != nil {
		return &agRes.GetEventStringBucketsResponse{}, err
	}

	totalNumberOfBucketsAllowed, err := getTotalNumberOfBucketsAllowed(request)
	if err != nil {
		return &agRes.GetEventStringBucketsResponse{}, err
	}

	eventStringCollection, err := collectEventGuitarStrings(ctx, eventFeedAggregate.feedServiceClient, request, totalNumberOfBucketsAllowed)
	if err != nil {
		return &agRes.GetEventStringBucketsResponse{}, err
	}

	return eventStringCollection, nil
}

func validateEventStringsRequest(request *agReq.GetEventStringBucketsRequest) error {
	// Validate TimeZone
	if request.Timezone == "" {
		return status.Error(codes.InvalidArgument, "A timezone must be provided")
	}

	_, err := time.LoadLocation(request.Timezone)
	if err != nil {
		return status.Error(codes.InvalidArgument, err.Error())
	}

	// Validate HoursBetween
	if request.HoursBetween <= 0 || request.HoursBetween > 24 {
		return status.Error(codes.InvalidArgument,
			"HoursBetween must be greater than 0 and less than or equal to 24")
	}

	if 24%request.HoursBetween != 0 {
		return status.Error(codes.InvalidArgument, "24 must be divisible by HoursBetween")
	}

	// Validate Date Range
	err = validateDateRange(request.Start, request.End)
	if err != nil {
		return err
	}

	return nil
}

func getTotalNumberOfBucketsAllowed(request *agReq.GetEventStringBucketsRequest) (int, error) {
	start := request.Start
	end := request.End
	bucketSizeInHours := int(request.HoursBetween)
	numberOfDaysBetween, err := getNumberOfDaysBetween(start, end, request.Timezone)
	if err != nil {
		return 0, err
	}

	return ((24 / bucketSizeInHours) * numberOfDaysBetween), nil
}

func getNumberOfDaysBetween(start, end, loc string) (int, error) {
	timezone, err := time.LoadLocation(loc)
	if err != nil {
		return 0, err
	}

	startTime, err := time.ParseInLocation("2006-01-02", start, timezone)
	if err != nil {
		return 0, err
	}

	endTime, err := time.ParseInLocation("2006-01-02", end, timezone)
	if err != nil {
		return 0, err
	}

	startUpdate := time.Date(startTime.Year(), startTime.Month(), startTime.Day(),
		0, 0, 0, 0, timezone)
	endUpdate := time.Date(endTime.Year(), endTime.Month(), endTime.Day(),
		0, 0, 0, 0, timezone).AddDate(0, 0, 1)
	return int(math.Round(endUpdate.Sub(startUpdate).Hours() / 24.0)), nil
}

// ValidateDateRange will validate that the provided start & end date are valid. That means they
// both have to have the right format and the start time must be less than or equal to the end time
//
// NOTE: If start or end are empty strings ("") that's consider an ok "empty" parameter
func validateDateRange(start string, end string) error {
	var (
		startTime time.Time
		endTime   time.Time
		err       error
	)

	if start != "" {
		startTime, err = createStartTime(start, time.UTC)
		if err != nil {
			return err
		}
	}

	if end != "" {
		endTime, err = createEndTime(end, time.UTC)
		if err != nil {
			return err
		}
	}

	// If both were provided, lets verify that the
	// end time is after the start time
	if end != "" && start != "" {
		if endTime.Equal(startTime) {
			return nil
		}
		if endTime.Before(startTime) {
			return status.Error(codes.InvalidArgument, "Invalid start time must be before end time")
		}
	}

	return nil
}

func eventFeedToGatewayEventCollection(slots []*event_feed_api.Timeslot, totalNumberOfBucketsAllowed int) []*agRes.EventCollection {
	newCollection := make([]*agRes.EventCollection, len(slots))
	for e, slot := range slots {
		newCollection[e] = new(agRes.EventCollection)
		if slot != nil && len(slot.Counts) > 0 {
			newCollection[e].EventsCount = make([]*agRes.EventCount, len(slot.Counts))
			for c, count := range slot.Counts {
				newCollection[e].EventsCount[c] = &agRes.EventCount{Name: count.Category, Count: count.Count}
			}
		}
	}

	if len(slots) < totalNumberOfBucketsAllowed {
		for index := len(slots); index < totalNumberOfBucketsAllowed; index++ {
			newCollection = append(newCollection, new(agRes.EventCollection))
		}
	}

	return newCollection[:totalNumberOfBucketsAllowed]
}

func collectEventGuitarStrings(ctx context.Context,
	feedServiceClient event_feed_api.EventFeedServiceClient,
	request *agReq.GetEventStringBucketsRequest, totalNumberOfBucketsAllowed int) (*agRes.GetEventStringBucketsResponse, error) {

	req := &event_feed_api.FeedTimelineRequest{
		Start:    request.GetStart(),
		End:      request.GetEnd(),
		Timezone: request.GetTimezone(),
		Interval: request.GetHoursBetween(),
		Filters:  request.GetFilter(),
	}

	feedTimeline, err := feedServiceClient.GetFeedTimeline(ctx, req)
	if err != nil {
		return nil, err
	}

	// convert into gateway string buckets
	agEventStringBuckets := make([]*agRes.EventString, len(feedTimeline.ActionLines))
	for index, line := range feedTimeline.ActionLines {
		agEventStringBuckets[index] = &agRes.EventString{
			Collection:  eventFeedToGatewayEventCollection(line.Slots, totalNumberOfBucketsAllowed),
			EventAction: line.Action,
		}
	}

	return &agRes.GetEventStringBucketsResponse{
		Strings:      agEventStringBuckets,
		Start:        feedTimeline.Start,
		End:          feedTimeline.End,
		HoursBetween: feedTimeline.Interval,
	}, nil
}

func createStartTime(startDate string, loc *time.Location) (time.Time, error) {
	startTime, err := time.ParseInLocation("2006-01-02", startDate, loc)
	if err != nil {
		return startTime, err
	}

	// At the beginning of the day
	return time.Date(startTime.Year(), startTime.Month(), startTime.Day(),
		0, 0, 0, 0, startTime.Location()), nil
}

func createEndTime(endDate string, loc *time.Location) (time.Time, error) {
	endTime, err := time.ParseInLocation("2006-01-02", endDate, loc)
	if err != nil {
		return endTime, err
	}

	// At the end of the day
	return time.Date(endTime.Year(), endTime.Month(), endTime.Day(),
		23, 59, 59, 0, endTime.Location()), nil
}
