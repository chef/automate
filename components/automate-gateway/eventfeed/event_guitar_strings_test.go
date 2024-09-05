package eventfeed_test

import (
	"context"
	"testing"

	agReq "github.com/chef/automate/api/external/event_feed/request"
	agRes "github.com/chef/automate/api/external/event_feed/response"
	event_feed_api "github.com/chef/automate/api/interservice/event_feed"
	subject "github.com/chef/automate/components/automate-gateway/eventfeed"
	"github.com/golang/mock/gomock"
	"github.com/stretchr/testify/assert"
)

func TestEventGuitarStringsNormal(t *testing.T) {
	start := "2018-02-11"
	end := "2018-02-11"
	var hoursBetween int32 = 6

	ctrl := gomock.NewController(t)

	mockFeedServiceClient := event_feed_api.NewMockEventFeedServiceClient(ctrl)
	mockFeedServiceClient.EXPECT().GetFeedTimeline(
		context.Background(),
		gomock.Any(),
	).DoAndReturn(func(c context.Context, request *event_feed_api.FeedTimelineRequest) (*event_feed_api.FeedTimelineResponse, error) {
		return &event_feed_api.FeedTimelineResponse{
			Start:    request.Start,
			End:      request.End,
			Interval: request.Interval,
			ActionLines: []*event_feed_api.ActionLine{
				{
					Action: "update",
					Slots:  make([]*event_feed_api.Timeslot, 3),
				},
				{
					Action: "create",
					Slots:  make([]*event_feed_api.Timeslot, 3),
				},
				{
					Action: "delete",
					Slots:  make([]*event_feed_api.Timeslot, 3),
				},
			},
		}, nil
	})

	eventFeedAggregate := subject.NewEventFeedAggregate(mockFeedServiceClient)

	eventGuitarStrings, err := eventFeedAggregate.CollectEventGuitarStrings(
		context.Background(),
		&agReq.GetEventStringBucketsRequest{
			Start:        start,
			End:          end,
			Timezone:     "America/Los_Angeles",
			HoursBetween: hoursBetween,
		},
	)

	assert.Nil(t, err)
	assert.Equal(t, 3, len(eventGuitarStrings.Strings))

	for _, guitarString := range eventGuitarStrings.Strings {
		assert.Equal(t, 0, len(guitarString.Collection[0].EventsCount))
		assert.Equal(t, 0, len(guitarString.Collection[1].EventsCount))
		assert.Equal(t, 0, len(guitarString.Collection[2].EventsCount))
	}

	assert.Equal(t, hoursBetween, eventGuitarStrings.HoursBetween)
	assert.Equal(t, start, eventGuitarStrings.Start)
	assert.Equal(t, end, eventGuitarStrings.End)
}

func TestEventGuitarStringsNotEnoughEventsStrings(t *testing.T) {
	start := "2018-02-11"
	end := "2018-02-11"
	var hoursBetween int32 = 6

	ctrl := gomock.NewController(t)

	mockFeedServiceClient := event_feed_api.NewMockEventFeedServiceClient(ctrl)
	mockFeedServiceClient.EXPECT().GetFeedTimeline(
		context.Background(),
		gomock.Any(),
	).DoAndReturn(func(c context.Context, request *event_feed_api.FeedTimelineRequest) (*event_feed_api.FeedTimelineResponse, error) {
		return &event_feed_api.FeedTimelineResponse{
			Start:    request.Start,
			End:      request.End,
			Interval: request.Interval,
			ActionLines: []*event_feed_api.ActionLine{
				{
					Action: "update",
					Slots:  make([]*event_feed_api.Timeslot, 4),
				},
				{
					Action: "create",
					Slots:  make([]*event_feed_api.Timeslot, 3), // error only 3 buckets
				},
				{
					Action: "delete",
					Slots:  make([]*event_feed_api.Timeslot, 4),
				},
			},
		}, nil
	})

	eventFeedAggregate := subject.NewEventFeedAggregate(mockFeedServiceClient)

	eventStrings, err := eventFeedAggregate.CollectEventGuitarStrings(
		context.Background(),
		&agReq.GetEventStringBucketsRequest{
			Start:        start,
			End:          end,
			Timezone:     "America/Los_Angeles",
			HoursBetween: hoursBetween,
		},
	)

	assert.Nil(t, err)

	for _, eventString := range eventStrings.Strings {
		assert.Equal(t, 4, len(eventString.Collection))
	}
}

func TestEventGuitarStringsToManyEventsStrings(t *testing.T) {
	start := "2018-03-11"
	end := "2018-03-11"
	var hoursBetween int32 = 6

	ctrl := gomock.NewController(t)

	mockFeedServiceClient := event_feed_api.NewMockEventFeedServiceClient(ctrl)
	mockFeedServiceClient.EXPECT().GetFeedTimeline(
		context.Background(),
		gomock.Any(),
	).DoAndReturn(func(c context.Context, request *event_feed_api.FeedTimelineRequest) (*event_feed_api.FeedTimelineResponse, error) {
		return &event_feed_api.FeedTimelineResponse{
			Start:    request.Start,
			End:      request.End,
			Interval: request.Interval,
			ActionLines: []*event_feed_api.ActionLine{
				{
					Action: "update",
					Slots:  make([]*event_feed_api.Timeslot, 4),
				},
				{
					Action: "create",
					Slots:  make([]*event_feed_api.Timeslot, 5), // error 5 buckets
				},
				{
					Action: "delete",
					Slots:  make([]*event_feed_api.Timeslot, 4),
				},
			},
		}, nil
	})

	eventFeedAggregate := subject.NewEventFeedAggregate(mockFeedServiceClient)

	eventStrings, err := eventFeedAggregate.CollectEventGuitarStrings(
		context.Background(),
		&agReq.GetEventStringBucketsRequest{
			Start:        start,
			End:          end,
			Timezone:     "America/Los_Angeles",
			HoursBetween: hoursBetween,
		},
	)

	assert.Nil(t, err)

	for _, eventString := range eventStrings.Strings {
		assert.Equal(t, 4, len(eventString.Collection))
	}
}

func TestEventGuitarStringsTimeZones(t *testing.T) {
	ctrl := gomock.NewController(t)
	mockFeedServiceClient := event_feed_api.NewMockEventFeedServiceClient(ctrl)

	eventFeedAggregate := subject.NewEventFeedAggregate(mockFeedServiceClient)

	cases := []*agReq.GetEventStringBucketsRequest{
		{
			Start:        "2018-01-01",
			End:          "2018-01-06",
			HoursBetween: 3,
			Timezone:     "",
		},
		{
			Start:        "2018-01-01",
			End:          "2018-01-06",
			HoursBetween: 3,
			Timezone:     "fake",
		},
		{
			Start:        "2018-01-01",
			End:          "2018-01-06",
			HoursBetween: 3,
			Timezone:     "America/Piqua",
		},
	}
	for _, eventStrings := range cases {
		_, err := eventFeedAggregate.CollectEventGuitarStrings(
			context.Background(),
			eventStrings,
		)

		assert.NotNil(t, err)
	}
}

func TestEventGuitarStringsStartAndEnd(t *testing.T) {
	ctrl := gomock.NewController(t)
	mockFeedServiceClient := event_feed_api.NewMockEventFeedServiceClient(ctrl)

	eventFeedAggregate := subject.NewEventFeedAggregate(mockFeedServiceClient)

	cases := []agReq.GetEventStringBucketsRequest{
		{
			Start:        "2018-01-06",
			End:          "2018-01-01",
			HoursBetween: 3,
			Timezone:     "UTC",
		},
		{
			Start:        "2000-00-00",
			End:          "2000-00-06",
			HoursBetween: 3,
			Timezone:     "UTC",
		},
		{
			Start:        "00-00-00",
			End:          "00-00-06",
			HoursBetween: 3,
			Timezone:     "UTC",
		},
		{
			Start:        "18-10-10",
			End:          "18-10-16",
			HoursBetween: 3,
			Timezone:     "UTC",
		},
		{
			Start:        "20-01-01",
			End:          "20-01-06",
			HoursBetween: 3,
			Timezone:     "UTC",
		},
		{
			Start:        "17:01:01",
			End:          "17:01:06",
			HoursBetween: 3,
			Timezone:     "UTC",
		},
		{
			Start:        "01-01-2000",
			End:          "06-01-2000",
			HoursBetween: 3,
			Timezone:     "UTC",
		},
		{
			Start:        "3000-12",
			End:          "3006-12",
			HoursBetween: 3,
			Timezone:     "UTC",
		},
		{
			Start:        "2019",
			End:          "2018",
			HoursBetween: 3,
			Timezone:     "UTC",
		},
		{
			Start:        "1888:01:01",
			End:          "1888:01:06",
			HoursBetween: 3,
			Timezone:     "UTC",
		},
		{
			Start:        "2027/01/01",
			End:          "2027/01/08",
			HoursBetween: 3,
			Timezone:     "UTC",
		},
	}
	for _, eventStrings := range cases {
		_, err := eventFeedAggregate.CollectEventGuitarStrings(
			context.Background(),
			&eventStrings,
		)

		assert.NotNil(t, err)
	}
}

func TestEventGuitarStringsHoursBetween(t *testing.T) {
	ctrl := gomock.NewController(t)
	mockFeedServiceClient := event_feed_api.NewMockEventFeedServiceClient(ctrl)

	eventFeedAggregate := subject.NewEventFeedAggregate(mockFeedServiceClient)
	cases := []int32{-1, 0, 5, 7, 9, 10, 11, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 25, 27, 98}

	for _, hoursBetween := range cases {
		_, err := eventFeedAggregate.CollectEventGuitarStrings(
			context.Background(),
			&agReq.GetEventStringBucketsRequest{
				Start:        "2018-01-01",
				End:          "2018-01-06",
				HoursBetween: hoursBetween,
				Timezone:     "UTC",
			},
		)

		assert.NotNil(t, err)
	}
}

type Params struct {
	t                  *testing.T
	eventAction        string
	eventType          string
	eventTypeCount     int
	bucketIndex        int
	eventGuitarStrings *agRes.GetEventStringBucketsResponse
}

func assertEventCount(p Params) {
	var (
		t                  = p.t
		eventAction        = p.eventAction
		eventType          = p.eventType
		eventTypeCount     = p.eventTypeCount
		bucketIndex        = p.bucketIndex
		eventGuitarStrings = p.eventGuitarStrings
	)

	matchingEventString, found := findGuitarString(eventGuitarStrings, eventAction)
	assert.True(t, found, "event string '%s' was not found", eventAction)

	if !found {
		return
	}

	assert.True(t, len(matchingEventString.Collection) > bucketIndex)

	matchingEventCount, found := findEventCount(matchingEventString.Collection[bucketIndex].EventsCount, eventType)
	assert.True(t, found, "event type '%s' was not found", eventType)

	if !found {
		return
	}

	assert.Equal(t, int64(eventTypeCount), matchingEventCount.Count)
}

func findGuitarString(eventStrings *agRes.GetEventStringBucketsResponse, eventAction string) (*agRes.EventString, bool) {
	for _, eventString := range eventStrings.Strings {
		if eventString.EventAction == eventAction {
			return eventString, true
		}
	}

	return nil, false
}

func findEventCount(eventCounts []*agRes.EventCount, eventType string) (*agRes.EventCount, bool) {
	for _, eventCount := range eventCounts {
		if eventCount.Name == eventType {
			return eventCount, true
		}
	}

	return nil, false
}
