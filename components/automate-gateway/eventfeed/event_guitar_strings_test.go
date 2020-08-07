package eventfeed_test

import (
	"context"
	"testing"

	cmsReq "github.com/chef/automate/api/interservice/cfgmgmt/request"
	cmsRes "github.com/chef/automate/api/interservice/cfgmgmt/response"
	cmsService "github.com/chef/automate/api/interservice/cfgmgmt/service"
	event_feed_api "github.com/chef/automate/api/interservice/event_feed"
	agReq "github.com/chef/automate/components/automate-gateway/api/event_feed/request"
	agRes "github.com/chef/automate/components/automate-gateway/api/event_feed/response"
	subject "github.com/chef/automate/components/automate-gateway/eventfeed"
	"github.com/golang/mock/gomock"
	"github.com/stretchr/testify/assert"
	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/status"
)

func TestEventGuitarStringsNormal(t *testing.T) {
	start := "2018-02-11"
	end := "2018-02-11"
	var hoursBetween int32 = 6

	ctrl := gomock.NewController(t)
	mockCfgMgmtClient := cmsService.NewMockCfgMgmtClient(ctrl)

	mockCfgMgmtClient.EXPECT().GetEventStringBuckets(
		context.Background(),
		gomock.Any(),
	).DoAndReturn(func(c context.Context, request *cmsReq.EventStrings) (*cmsRes.EventStrings, error) {
		return &cmsRes.EventStrings{
			Start:        request.Start,
			End:          request.End,
			HoursBetween: request.HoursBetween,
			Strings: []*cmsRes.EventString{
				{
					EventAction: "update",
					Collection:  make([]*cmsRes.EventCollection, 3),
				},
				{
					EventAction: "create",
					Collection:  make([]*cmsRes.EventCollection, 3),
				},
				{
					EventAction: "delete",
					Collection:  make([]*cmsRes.EventCollection, 3),
				},
			},
		}, nil
	})

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

	eventFeedAggregate := subject.NewEventFeedAggregate(mockCfgMgmtClient, mockFeedServiceClient)

	eventGuitarStrings, err := eventFeedAggregate.CollectEventGuitarStrings(
		context.Background(),
		&agReq.EventStrings{
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
	mockCfgMgmtClient := cmsService.NewMockCfgMgmtClient(ctrl)

	mockCfgMgmtClient.EXPECT().GetEventStringBuckets(
		context.Background(),
		gomock.Any(),
	).DoAndReturn(func(c context.Context, request *cmsReq.EventStrings) (*cmsRes.EventStrings, error) {
		return &cmsRes.EventStrings{
			Start:        request.Start,
			End:          request.End,
			HoursBetween: request.HoursBetween,
			Strings: []*cmsRes.EventString{
				{
					EventAction: "update",
					Collection:  make([]*cmsRes.EventCollection, 4),
				},
				{
					EventAction: "create",
					Collection:  make([]*cmsRes.EventCollection, 3), // error only 3 buckets
				},
				{
					EventAction: "delete",
					Collection:  make([]*cmsRes.EventCollection, 4),
				},
			},
		}, nil
	})

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
					Slots:  make([]*event_feed_api.Timeslot, 4),
				},
				{
					Action: "delete",
					Slots:  make([]*event_feed_api.Timeslot, 4),
				},
			},
		}, nil
	})

	eventFeedAggregate := subject.NewEventFeedAggregate(mockCfgMgmtClient, mockFeedServiceClient)

	eventStrings, err := eventFeedAggregate.CollectEventGuitarStrings(
		context.Background(),
		&agReq.EventStrings{
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
	mockCfgMgmtClient := cmsService.NewMockCfgMgmtClient(ctrl)

	mockCfgMgmtClient.EXPECT().GetEventStringBuckets(
		context.Background(),
		gomock.Any(),
	).DoAndReturn(func(c context.Context, request *cmsReq.EventStrings) (*cmsRes.EventStrings, error) {
		return &cmsRes.EventStrings{
			Start:        request.Start,
			End:          request.End,
			HoursBetween: request.HoursBetween,
			Strings: []*cmsRes.EventString{
				{
					EventAction: "update",
					Collection:  make([]*cmsRes.EventCollection, 4),
				},
				{
					EventAction: "create",
					Collection:  make([]*cmsRes.EventCollection, 5), // error only 3 buckets
				},
				{
					EventAction: "delete",
					Collection:  make([]*cmsRes.EventCollection, 4),
				},
			},
		}, nil
	})

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
					Slots:  make([]*event_feed_api.Timeslot, 4),
				},
				{
					Action: "delete",
					Slots:  make([]*event_feed_api.Timeslot, 4),
				},
			},
		}, nil
	})

	eventFeedAggregate := subject.NewEventFeedAggregate(mockCfgMgmtClient, mockFeedServiceClient)

	eventStrings, err := eventFeedAggregate.CollectEventGuitarStrings(
		context.Background(),
		&agReq.EventStrings{
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

func TestEventGuitarStringsMergeOpenSlots(t *testing.T) {
	start := "2018-02-11"
	end := "2018-02-11"
	var hoursBetween int32 = 6

	ctrl := gomock.NewController(t)
	mockCfgMgmtClient := cmsService.NewMockCfgMgmtClient(ctrl)

	mockCfgMgmtClient.EXPECT().GetEventStringBuckets(
		context.Background(),
		gomock.Any(),
	).DoAndReturn(func(c context.Context, request *cmsReq.EventStrings) (*cmsRes.EventStrings, error) {
		return &cmsRes.EventStrings{
			Start:        request.Start,
			End:          request.End,
			HoursBetween: request.HoursBetween,
			Strings: []*cmsRes.EventString{
				{
					EventAction: "update",
					Collection: []*cmsRes.EventCollection{
						{}, {
							EventsCount: []*cmsRes.EventCount{
								{Name: "node", Count: 1},
							},
						},
						{}, {},
					},
				},
				{
					EventAction: "create",
					Collection: []*cmsRes.EventCollection{
						{}, {},
						{
							EventsCount: []*cmsRes.EventCount{
								{Name: "cookbook", Count: 1},
							},
						}, {},
					},
				},
				{
					EventAction: "delete",
					Collection: []*cmsRes.EventCollection{
						{}, {
							EventsCount: []*cmsRes.EventCount{
								{Name: "item", Count: 1},
							},
						},
						{}, {
							EventsCount: []*cmsRes.EventCount{
								{Name: "bag", Count: 1},
							},
						},
					},
				},
			},
		}, nil
	})

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
					Slots: []*event_feed_api.Timeslot{
						{
							Counts: []*event_feed_api.EntryCount{
								{Category: "scanjobs", Count: 1},
							},
						},
						{}, {}, {},
					},
				},
				{
					Action: "create",
					Slots: []*event_feed_api.Timeslot{
						{}, {}, {},
						{
							Counts: []*event_feed_api.EntryCount{
								{Category: "profile", Count: 1},
							},
						},
					},
				},
				{
					Action: "delete",
					Slots: []*event_feed_api.Timeslot{
						{}, {},
						{
							Counts: []*event_feed_api.EntryCount{
								{Category: "scanjobs", Count: 1},
							},
						}, {},
					},
				},
			},
		}, nil
	})

	eventFeedAggregate := subject.NewEventFeedAggregate(mockCfgMgmtClient, mockFeedServiceClient)

	eventGuitarStrings, err := eventFeedAggregate.CollectEventGuitarStrings(
		context.Background(),
		&agReq.EventStrings{
			Start:        start,
			End:          end,
			Timezone:     "America/Los_Angeles",
			HoursBetween: hoursBetween,
		},
	)

	assert.Nil(t, err)
	assert.Equal(t, 3, len(eventGuitarStrings.Strings))

	assertEventCount(Params{t: t, eventAction: "update", eventType: "scanjobs", eventTypeCount: 1, bucketIndex: 0, eventGuitarStrings: eventGuitarStrings})
	assertEventCount(Params{t: t, eventAction: "update", eventType: "node", eventTypeCount: 1, bucketIndex: 1, eventGuitarStrings: eventGuitarStrings})

	assertEventCount(Params{t: t, eventAction: "create", eventType: "cookbook", eventTypeCount: 1, bucketIndex: 2, eventGuitarStrings: eventGuitarStrings})
	assertEventCount(Params{t: t, eventAction: "create", eventType: "profile", eventTypeCount: 1, bucketIndex: 3, eventGuitarStrings: eventGuitarStrings})

	assertEventCount(Params{t: t, eventAction: "delete", eventType: "item", eventTypeCount: 1, bucketIndex: 1, eventGuitarStrings: eventGuitarStrings})
	assertEventCount(Params{t: t, eventAction: "delete", eventType: "scanjobs", eventTypeCount: 1, bucketIndex: 2, eventGuitarStrings: eventGuitarStrings})
	assertEventCount(Params{t: t, eventAction: "delete", eventType: "bag", eventTypeCount: 1, bucketIndex: 3, eventGuitarStrings: eventGuitarStrings})

	assert.Equal(t, hoursBetween, eventGuitarStrings.HoursBetween)
	assert.Equal(t, start, eventGuitarStrings.Start)
	assert.Equal(t, end, eventGuitarStrings.End)
}

func TestEventGuitarStringsMergeMultipleEventTypesAndCounts(t *testing.T) {
	start := "2018-02-11"
	end := "2018-02-11"
	var hoursBetween int32 = 6

	ctrl := gomock.NewController(t)
	mockCfgMgmtClient := cmsService.NewMockCfgMgmtClient(ctrl)

	mockCfgMgmtClient.EXPECT().GetEventStringBuckets(
		context.Background(),
		gomock.Any(),
	).DoAndReturn(func(c context.Context, request *cmsReq.EventStrings) (*cmsRes.EventStrings, error) {
		return &cmsRes.EventStrings{
			Start:        request.Start,
			End:          request.End,
			HoursBetween: request.HoursBetween,
			Strings: []*cmsRes.EventString{
				{
					EventAction: "update",
					Collection: []*cmsRes.EventCollection{
						{
							EventsCount: []*cmsRes.EventCount{
								{Name: "node", Count: 5},
							},
						}, {}, {}, {},
					},
				},
				{
					EventAction: "create",
					Collection: []*cmsRes.EventCollection{
						{}, {}, {},
						{
							EventsCount: []*cmsRes.EventCount{
								{Name: "cookbook", Count: 100},
							},
						},
					},
				},
				{
					EventAction: "delete",
					Collection: []*cmsRes.EventCollection{
						{}, {},
						{
							EventsCount: []*cmsRes.EventCount{
								{Name: "environment", Count: 999},
								{Name: "role", Count: 450},
								{Name: "cookbook", Count: 20},
								{Name: "bag", Count: 5},
								{Name: "scanjobs", Count: 100},
							},
						}, {},
					},
				},
			},
		}, nil
	})

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
					Slots: []*event_feed_api.Timeslot{
						{
							Counts: []*event_feed_api.EntryCount{
								{Category: "scanjobs", Count: 10},
							},
						}, {}, {}, {},
					},
				},
				{
					Action: "create",
					Slots: []*event_feed_api.Timeslot{
						{}, {}, {},
						{
							Counts: []*event_feed_api.EntryCount{
								{Category: "profile", Count: 50},
							},
						},
					},
				},
				{
					Action: "delete",
					Slots: []*event_feed_api.Timeslot{
						{}, {},
						{
							Counts: []*event_feed_api.EntryCount{
								{Category: "scanjobs", Count: 500},
								{Category: "profile", Count: 100},
							},
						}, {},
					},
				},
			},
		}, nil
	})

	eventFeedAggregate := subject.NewEventFeedAggregate(mockCfgMgmtClient, mockFeedServiceClient)

	eventGuitarStrings, err := eventFeedAggregate.CollectEventGuitarStrings(
		context.Background(),
		&agReq.EventStrings{
			Start:        start,
			End:          end,
			Timezone:     "America/Los_Angeles",
			HoursBetween: hoursBetween,
		},
	)

	assert.Nil(t, err)
	assert.Equal(t, 3, len(eventGuitarStrings.Strings))

	assertEventCount(Params{t: t, eventAction: "update", eventType: "scanjobs", eventTypeCount: 10, bucketIndex: 0, eventGuitarStrings: eventGuitarStrings})
	assertEventCount(Params{t: t, eventAction: "update", eventType: "node", eventTypeCount: 5, bucketIndex: 0, eventGuitarStrings: eventGuitarStrings})

	assertEventCount(Params{t: t, eventAction: "create", eventType: "cookbook", eventTypeCount: 100, bucketIndex: 3, eventGuitarStrings: eventGuitarStrings})
	assertEventCount(Params{t: t, eventAction: "create", eventType: "profile", eventTypeCount: 50, bucketIndex: 3, eventGuitarStrings: eventGuitarStrings})

	assertEventCount(Params{t: t, eventAction: "delete", eventType: "scanjobs", eventTypeCount: 600, bucketIndex: 2, eventGuitarStrings: eventGuitarStrings})
	assertEventCount(Params{t: t, eventAction: "delete", eventType: "profile", eventTypeCount: 100, bucketIndex: 2, eventGuitarStrings: eventGuitarStrings})
	assertEventCount(Params{t: t, eventAction: "delete", eventType: "environment", eventTypeCount: 999, bucketIndex: 2, eventGuitarStrings: eventGuitarStrings})
	assertEventCount(Params{t: t, eventAction: "delete", eventType: "role", eventTypeCount: 450, bucketIndex: 2, eventGuitarStrings: eventGuitarStrings})
	assertEventCount(Params{t: t, eventAction: "delete", eventType: "cookbook", eventTypeCount: 20, bucketIndex: 2, eventGuitarStrings: eventGuitarStrings})
	assertEventCount(Params{t: t, eventAction: "delete", eventType: "bag", eventTypeCount: 5, bucketIndex: 2, eventGuitarStrings: eventGuitarStrings})

	assert.Equal(t, hoursBetween, eventGuitarStrings.HoursBetween)
	assert.Equal(t, start, eventGuitarStrings.Start)
	assert.Equal(t, end, eventGuitarStrings.End)
}

func TestEventGuitarStringsMergeBugAIA222(t *testing.T) {
	start := "2018-02-11"
	end := "2018-02-11"
	var hoursBetween int32 = 6

	ctrl := gomock.NewController(t)
	mockCfgMgmtClient := cmsService.NewMockCfgMgmtClient(ctrl)

	mockCfgMgmtClient.EXPECT().GetEventStringBuckets(
		context.Background(),
		gomock.Any(),
	).DoAndReturn(func(c context.Context, request *cmsReq.EventStrings) (*cmsRes.EventStrings, error) {
		return &cmsRes.EventStrings{
			Start:        request.Start,
			End:          request.End,
			HoursBetween: request.HoursBetween,
			Strings: []*cmsRes.EventString{
				{
					EventAction: "update",
					Collection: []*cmsRes.EventCollection{
						{}, {}, {}, {},
					},
				},
				{
					EventAction: "create",
					Collection: []*cmsRes.EventCollection{
						{}, {}, {},
						{},
					},
				},
				{
					EventAction: "delete",
					Collection: []*cmsRes.EventCollection{
						{}, {},
						{
							EventsCount: []*cmsRes.EventCount{
								{Name: "profile", Count: 1},
							},
						}, {},
					},
				},
			},
		}, nil
	})

	mockFeedServiceClient := event_feed_api.NewMockEventFeedServiceClient(ctrl)
	mockFeedServiceClient.EXPECT().GetFeedTimeline(
		context.Background(),
		gomock.Any(),
	).DoAndReturn(func(c context.Context, request *event_feed_api.FeedTimelineRequest) (*event_feed_api.FeedTimelineResponse, error) {
		return &event_feed_api.FeedTimelineResponse{}, status.Error(codes.Unavailable, "compliance service not running")
	})

	eventFeedAggregate := subject.NewEventFeedAggregate(mockCfgMgmtClient, mockFeedServiceClient)

	eventGuitarStrings, err := eventFeedAggregate.CollectEventGuitarStrings(
		context.Background(),
		&agReq.EventStrings{
			Start:        start,
			End:          end,
			Timezone:     "America/Los_Angeles",
			HoursBetween: hoursBetween,
		},
	)

	assert.Nil(t, err)
	assert.Equal(t, 3, len(eventGuitarStrings.Strings))

	for _, guitarString := range eventGuitarStrings.Strings {
		if guitarString.EventAction == "update" {
			assert.Equal(t, 0, len(guitarString.Collection[0].EventsCount))
			assert.Equal(t, 0, len(guitarString.Collection[1].EventsCount))
			assert.Equal(t, 0, len(guitarString.Collection[2].EventsCount))
			assert.Equal(t, 0, len(guitarString.Collection[3].EventsCount))
		} else if guitarString.EventAction == "create" {
			assert.Equal(t, 0, len(guitarString.Collection[0].EventsCount))
			assert.Equal(t, 0, len(guitarString.Collection[1].EventsCount))
			assert.Equal(t, 0, len(guitarString.Collection[2].EventsCount))
			assert.Equal(t, 0, len(guitarString.Collection[3].EventsCount))
		} else if guitarString.EventAction == "delete" {
			assert.Equal(t, 0, len(guitarString.Collection[0].EventsCount))
			assert.Equal(t, 0, len(guitarString.Collection[1].EventsCount))
			assert.Equal(t, 1, len(guitarString.Collection[2].EventsCount))
			assert.Equal(t, int64(1), guitarString.Collection[2].EventsCount[0].Count)
			assert.Equal(t, "profile", guitarString.Collection[2].EventsCount[0].Name)
			assert.Equal(t, 0, len(guitarString.Collection[3].EventsCount))
		}
	}

	assert.Equal(t, hoursBetween, eventGuitarStrings.HoursBetween)
	assert.Equal(t, start, eventGuitarStrings.Start)
	assert.Equal(t, end, eventGuitarStrings.End)
}

func TestEventGuitarStringsMissingString(t *testing.T) {
	start := "2018-02-11"
	end := "2018-02-11"
	var hoursBetween int32 = 6

	ctrl := gomock.NewController(t)
	mockCfgMgmtClient := cmsService.NewMockCfgMgmtClient(ctrl)

	mockCfgMgmtClient.EXPECT().GetEventStringBuckets(
		context.Background(),
		gomock.Any(),
	).DoAndReturn(func(c context.Context, request *cmsReq.EventStrings) (*cmsRes.EventStrings, error) {
		return &cmsRes.EventStrings{
			Start:        request.Start,
			End:          request.End,
			HoursBetween: request.HoursBetween,
			Strings: []*cmsRes.EventString{
				{
					EventAction: "update",
					Collection:  make([]*cmsRes.EventCollection, 4),
				},
				// &cmsRes.EventString{
				// 	EventAction: "create",   // the missing string
				//  Collection:  make([]*cmsRes.EventCollection, 4),
				// },
				{
					EventAction: "delete",
					Collection:  make([]*cmsRes.EventCollection, 4),
				},
			},
		}, nil
	})

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
					Slots:  make([]*event_feed_api.Timeslot, 4),
				},
				{
					Action: "delete",
					Slots:  make([]*event_feed_api.Timeslot, 4),
				},
			},
		}, nil
	})

	eventFeedAggregate := subject.NewEventFeedAggregate(mockCfgMgmtClient, mockFeedServiceClient)

	_, err := eventFeedAggregate.CollectEventGuitarStrings(
		context.Background(),
		&agReq.EventStrings{
			Start:        start,
			End:          end,
			Timezone:     "America/Los_Angeles",
			HoursBetween: hoursBetween,
		},
	)

	assert.NotNil(t, err)
}

func TestEventGuitarStringsDuplicateString1(t *testing.T) {
	start := "2018-02-11"
	end := "2018-02-11"
	var hoursBetween int32 = 6

	ctrl := gomock.NewController(t)
	mockCfgMgmtClient := cmsService.NewMockCfgMgmtClient(ctrl)

	mockCfgMgmtClient.EXPECT().GetEventStringBuckets(
		context.Background(),
		gomock.Any(),
	).DoAndReturn(func(c context.Context, request *cmsReq.EventStrings) (*cmsRes.EventStrings, error) {
		return &cmsRes.EventStrings{
			Start:        request.Start,
			End:          request.End,
			HoursBetween: request.HoursBetween,
			Strings: []*cmsRes.EventString{
				{
					EventAction: "update",
					Collection:  make([]*cmsRes.EventCollection, 4),
				},
				{
					EventAction: "update", // the Duplicate string
					Collection:  make([]*cmsRes.EventCollection, 4),
				},
				{
					EventAction: "delete",
					Collection:  make([]*cmsRes.EventCollection, 4),
				},
			},
		}, nil
	})

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
					Slots:  make([]*event_feed_api.Timeslot, 4),
				},
				{
					Action: "delete",
					Slots:  make([]*event_feed_api.Timeslot, 4),
				},
			},
		}, nil
	})

	eventFeedAggregate := subject.NewEventFeedAggregate(mockCfgMgmtClient, mockFeedServiceClient)

	_, err := eventFeedAggregate.CollectEventGuitarStrings(
		context.Background(),
		&agReq.EventStrings{
			Start:        start,
			End:          end,
			Timezone:     "America/Los_Angeles",
			HoursBetween: hoursBetween,
		},
	)

	assert.NotNil(t, err)
}

func TestEventGuitarStringsDuplicateString2(t *testing.T) {
	start := "2018-02-11"
	end := "2018-02-11"
	var hoursBetween int32 = 6

	ctrl := gomock.NewController(t)
	mockCfgMgmtClient := cmsService.NewMockCfgMgmtClient(ctrl)

	mockCfgMgmtClient.EXPECT().GetEventStringBuckets(
		context.Background(),
		gomock.Any(),
	).DoAndReturn(func(c context.Context, request *cmsReq.EventStrings) (*cmsRes.EventStrings, error) {
		return &cmsRes.EventStrings{
			Start:        request.Start,
			End:          request.End,
			HoursBetween: request.HoursBetween,
			Strings: []*cmsRes.EventString{
				{
					EventAction: "update",
					Collection:  make([]*cmsRes.EventCollection, 4),
				},
				{
					EventAction: "create",
					Collection:  make([]*cmsRes.EventCollection, 4),
				},
				{
					EventAction: "delete",
					Collection:  make([]*cmsRes.EventCollection, 4),
				},
			},
		}, nil
	})

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
					Action: "update", // the Duplicate string
					Slots:  make([]*event_feed_api.Timeslot, 4),
				},
				{
					Action: "delete",
					Slots:  make([]*event_feed_api.Timeslot, 4),
				},
			},
		}, nil
	})

	eventFeedAggregate := subject.NewEventFeedAggregate(mockCfgMgmtClient, mockFeedServiceClient)

	_, err := eventFeedAggregate.CollectEventGuitarStrings(
		context.Background(),
		&agReq.EventStrings{
			Start:        start,
			End:          end,
			Timezone:     "America/Los_Angeles",
			HoursBetween: hoursBetween,
		},
	)

	assert.NotNil(t, err)
}

func TestEventGuitarStringsConfigMgmtDown(t *testing.T) {
	start := "2018-02-11"
	end := "2018-02-11"
	var hoursBetween int32 = 6

	ctrl := gomock.NewController(t)
	mockCfgMgmtClient := cmsService.NewMockCfgMgmtClient(ctrl)

	mockCfgMgmtClient.EXPECT().GetEventStringBuckets(
		context.Background(),
		gomock.Any(),
	).DoAndReturn(func(c context.Context, request *cmsReq.EventStrings) (*cmsRes.EventStrings, error) {
		return &cmsRes.EventStrings{}, status.Error(codes.Unavailable, "cfgmgmt service not running")
	})

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
					Slots: []*event_feed_api.Timeslot{
						{
							Counts: []*event_feed_api.EntryCount{
								{Category: "scanjobs", Count: 1},
							},
						},
						{}, {},
						{},
					},
				},
				{
					Action: "create",
					Slots: []*event_feed_api.Timeslot{
						{}, {},
						{
							Counts: []*event_feed_api.EntryCount{
								{Category: "scanjobs", Count: 2},
							},
						},
						{},
					},
				},
				{
					Action: "delete",
					Slots: []*event_feed_api.Timeslot{
						{}, {},
						{}, {
							Counts: []*event_feed_api.EntryCount{
								{Category: "scanjobs", Count: 3},
							},
						},
					},
				},
			},
		}, nil
	})

	eventFeedAggregate := subject.NewEventFeedAggregate(mockCfgMgmtClient, mockFeedServiceClient)

	eventGuitarStrings, err := eventFeedAggregate.CollectEventGuitarStrings(
		context.Background(),
		&agReq.EventStrings{
			Start:        start,
			End:          end,
			Timezone:     "America/Los_Angeles",
			HoursBetween: hoursBetween,
		},
	)

	assert.Nil(t, err)
	assert.Equal(t, 3, len(eventGuitarStrings.Strings))

	assertEventCount(Params{t: t, eventAction: "update", eventType: "scanjobs", eventTypeCount: 1, bucketIndex: 0, eventGuitarStrings: eventGuitarStrings})

	assertEventCount(Params{t: t, eventAction: "create", eventType: "scanjobs", eventTypeCount: 2, bucketIndex: 2, eventGuitarStrings: eventGuitarStrings})

	assertEventCount(Params{t: t, eventAction: "delete", eventType: "scanjobs", eventTypeCount: 3, bucketIndex: 3, eventGuitarStrings: eventGuitarStrings})

	assert.Equal(t, hoursBetween, eventGuitarStrings.HoursBetween)
	assert.Equal(t, start, eventGuitarStrings.Start)
	assert.Equal(t, end, eventGuitarStrings.End)
}

func TestEventGuitarStringsComplianceDown(t *testing.T) {
	start := "2018-02-11"
	end := "2018-02-11"
	var hoursBetween int32 = 6

	ctrl := gomock.NewController(t)
	mockCfgMgmtClient := cmsService.NewMockCfgMgmtClient(ctrl)

	mockCfgMgmtClient.EXPECT().GetEventStringBuckets(
		context.Background(),
		gomock.Any(),
	).DoAndReturn(func(c context.Context, request *cmsReq.EventStrings) (*cmsRes.EventStrings, error) {
		return &cmsRes.EventStrings{
			Start:        request.Start,
			End:          request.End,
			HoursBetween: request.HoursBetween,
			Strings: []*cmsRes.EventString{
				{
					EventAction: "update",
					Collection: []*cmsRes.EventCollection{
						{
							EventsCount: []*cmsRes.EventCount{
								{Name: "bag", Count: 1},
							},
						},
						{}, {},
						{},
					},
				},
				{
					EventAction: "create",
					Collection: []*cmsRes.EventCollection{
						{}, {},
						{
							EventsCount: []*cmsRes.EventCount{
								{Name: "cookbook", Count: 2},
							},
						},
						{},
					},
				},
				{
					EventAction: "delete",
					Collection: []*cmsRes.EventCollection{
						{}, {},
						{}, {
							EventsCount: []*cmsRes.EventCount{
								{Name: "node", Count: 3},
							},
						},
					},
				},
			},
		}, nil
	})

	mockFeedServiceClient := event_feed_api.NewMockEventFeedServiceClient(ctrl)
	mockFeedServiceClient.EXPECT().GetFeedTimeline(
		context.Background(),
		gomock.Any(),
	).DoAndReturn(func(c context.Context, request *event_feed_api.FeedTimelineRequest) (*event_feed_api.FeedTimelineResponse, error) {
		return &event_feed_api.FeedTimelineResponse{}, status.Error(codes.Unavailable, "compliance service not running")
	})

	eventFeedAggregate := subject.NewEventFeedAggregate(mockCfgMgmtClient, mockFeedServiceClient)

	eventGuitarStrings, err := eventFeedAggregate.CollectEventGuitarStrings(
		context.Background(),
		&agReq.EventStrings{
			Start:        start,
			End:          end,
			Timezone:     "America/Los_Angeles",
			HoursBetween: hoursBetween,
		},
	)

	assert.Nil(t, err)
	assert.Equal(t, 3, len(eventGuitarStrings.Strings))

	assertEventCount(Params{t: t, eventAction: "update", eventType: "bag", eventTypeCount: 1, bucketIndex: 0, eventGuitarStrings: eventGuitarStrings})

	assertEventCount(Params{t: t, eventAction: "create", eventType: "cookbook", eventTypeCount: 2, bucketIndex: 2, eventGuitarStrings: eventGuitarStrings})

	assertEventCount(Params{t: t, eventAction: "delete", eventType: "node", eventTypeCount: 3, bucketIndex: 3, eventGuitarStrings: eventGuitarStrings})

	assert.Equal(t, hoursBetween, eventGuitarStrings.HoursBetween)
	assert.Equal(t, start, eventGuitarStrings.Start)
	assert.Equal(t, end, eventGuitarStrings.End)
}

func TestEventGuitarStringsAllSubServicesDown(t *testing.T) {
	start := "2018-02-11"
	end := "2018-02-11"
	var hoursBetween int32 = 6

	ctrl := gomock.NewController(t)
	mockCfgMgmtClient := cmsService.NewMockCfgMgmtClient(ctrl)

	mockCfgMgmtClient.EXPECT().GetEventStringBuckets(
		context.Background(),
		gomock.Any(),
	).DoAndReturn(func(c context.Context, request *cmsReq.EventStrings) (*cmsRes.EventStrings, error) {
		return &cmsRes.EventStrings{}, status.Error(codes.Unavailable, "cfgmgmt service not running")
	})

	mockFeedServiceClient := event_feed_api.NewMockEventFeedServiceClient(ctrl)
	mockFeedServiceClient.EXPECT().GetFeedTimeline(
		context.Background(),
		gomock.Any(),
	).DoAndReturn(func(c context.Context, request *event_feed_api.FeedTimelineRequest) (*event_feed_api.FeedTimelineResponse, error) {
		return &event_feed_api.FeedTimelineResponse{}, status.Error(codes.Unavailable, "compliance service not running")
	})

	eventFeedAggregate := subject.NewEventFeedAggregate(mockCfgMgmtClient, mockFeedServiceClient)

	eventGuitarStrings, err := eventFeedAggregate.CollectEventGuitarStrings(
		context.Background(),
		&agReq.EventStrings{
			Start:        start,
			End:          end,
			Timezone:     "America/Los_Angeles",
			HoursBetween: hoursBetween,
		},
	)

	assert.NotNil(t, err)
	assert.Contains(t, err.Error(), "cfgmgmt service not running")
	assert.Contains(t, err.Error(), "compliance service not running")
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

func TestEventGuitarStringsTimeZones(t *testing.T) {
	ctrl := gomock.NewController(t)
	mockCfgMgmtClient := cmsService.NewMockCfgMgmtClient(ctrl)
	mockFeedServiceClient := event_feed_api.NewMockEventFeedServiceClient(ctrl)

	eventFeedAggregate := subject.NewEventFeedAggregate(mockCfgMgmtClient, mockFeedServiceClient)

	cases := []*agReq.EventStrings{
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
	mockCfgMgmtClient := cmsService.NewMockCfgMgmtClient(ctrl)
	mockFeedServiceClient := event_feed_api.NewMockEventFeedServiceClient(ctrl)

	eventFeedAggregate := subject.NewEventFeedAggregate(mockCfgMgmtClient, mockFeedServiceClient)

	cases := []agReq.EventStrings{
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
	mockCfgMgmtClient := cmsService.NewMockCfgMgmtClient(ctrl)
	mockFeedServiceClient := event_feed_api.NewMockEventFeedServiceClient(ctrl)

	eventFeedAggregate := subject.NewEventFeedAggregate(mockCfgMgmtClient, mockFeedServiceClient)
	cases := []int32{-1, 0, 5, 7, 9, 10, 11, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 25, 27, 98}

	for _, hoursBetween := range cases {
		_, err := eventFeedAggregate.CollectEventGuitarStrings(
			context.Background(),
			&agReq.EventStrings{
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
	eventGuitarStrings *agRes.EventStrings
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

func findGuitarString(eventStrings *agRes.EventStrings, eventAction string) (*agRes.EventString, bool) {
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
