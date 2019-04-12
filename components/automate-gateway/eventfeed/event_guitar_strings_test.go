package eventfeed_test

import (
	"context"
	"testing"

	cmsReq "github.com/chef/automate/api/interservice/cfgmgmt/request"
	cmsRes "github.com/chef/automate/api/interservice/cfgmgmt/response"
	cmsService "github.com/chef/automate/api/interservice/cfgmgmt/service"
	agReq "github.com/chef/automate/components/automate-gateway/api/event_feed/request"
	agRes "github.com/chef/automate/components/automate-gateway/api/event_feed/response"
	subject "github.com/chef/automate/components/automate-gateway/eventfeed"
	mock_automate_feed "github.com/chef/automate/components/automate-gateway/gateway_mocks/mock_feed"
	complFeed "github.com/chef/automate/components/compliance-service/api/automate-feed"
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
				&cmsRes.EventString{
					EventAction: "update",
					Collection:  make([]*cmsRes.EventCollection, 3),
				},
				&cmsRes.EventString{
					EventAction: "create",
					Collection:  make([]*cmsRes.EventCollection, 3),
				},
				&cmsRes.EventString{
					EventAction: "delete",
					Collection:  make([]*cmsRes.EventCollection, 3),
				},
			},
		}, nil
	})

	mockFeedServiceClient := mock_automate_feed.NewMockFeedServiceClient(ctrl)
	mockFeedServiceClient.EXPECT().GetFeedTimeline(
		context.Background(),
		gomock.Any(),
	).DoAndReturn(func(c context.Context, request *complFeed.FeedTimelineRequest) (*complFeed.FeedTimelineResponse, error) {
		return &complFeed.FeedTimelineResponse{
			Start:    request.Start,
			End:      request.End,
			Interval: request.Interval,
			ActionLines: []*complFeed.ActionLine{
				&complFeed.ActionLine{
					Action: "update",
					Slots:  make([]*complFeed.Timeslot, 3),
				},
				&complFeed.ActionLine{
					Action: "create",
					Slots:  make([]*complFeed.Timeslot, 3),
				},
				&complFeed.ActionLine{
					Action: "delete",
					Slots:  make([]*complFeed.Timeslot, 3),
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
				&cmsRes.EventString{
					EventAction: "update",
					Collection:  make([]*cmsRes.EventCollection, 4),
				},
				&cmsRes.EventString{
					EventAction: "create",
					Collection:  make([]*cmsRes.EventCollection, 3), // error only 3 buckets
				},
				&cmsRes.EventString{
					EventAction: "delete",
					Collection:  make([]*cmsRes.EventCollection, 4),
				},
			},
		}, nil
	})

	mockFeedServiceClient := mock_automate_feed.NewMockFeedServiceClient(ctrl)
	mockFeedServiceClient.EXPECT().GetFeedTimeline(
		context.Background(),
		gomock.Any(),
	).DoAndReturn(func(c context.Context, request *complFeed.FeedTimelineRequest) (*complFeed.FeedTimelineResponse, error) {
		return &complFeed.FeedTimelineResponse{
			Start:    request.Start,
			End:      request.End,
			Interval: request.Interval,
			ActionLines: []*complFeed.ActionLine{
				&complFeed.ActionLine{
					Action: "update",
					Slots:  make([]*complFeed.Timeslot, 4),
				},
				&complFeed.ActionLine{
					Action: "create",
					Slots:  make([]*complFeed.Timeslot, 4),
				},
				&complFeed.ActionLine{
					Action: "delete",
					Slots:  make([]*complFeed.Timeslot, 4),
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
				&cmsRes.EventString{
					EventAction: "update",
					Collection:  make([]*cmsRes.EventCollection, 4),
				},
				&cmsRes.EventString{
					EventAction: "create",
					Collection:  make([]*cmsRes.EventCollection, 5), // error only 3 buckets
				},
				&cmsRes.EventString{
					EventAction: "delete",
					Collection:  make([]*cmsRes.EventCollection, 4),
				},
			},
		}, nil
	})

	mockFeedServiceClient := mock_automate_feed.NewMockFeedServiceClient(ctrl)
	mockFeedServiceClient.EXPECT().GetFeedTimeline(
		context.Background(),
		gomock.Any(),
	).DoAndReturn(func(c context.Context, request *complFeed.FeedTimelineRequest) (*complFeed.FeedTimelineResponse, error) {
		return &complFeed.FeedTimelineResponse{
			Start:    request.Start,
			End:      request.End,
			Interval: request.Interval,
			ActionLines: []*complFeed.ActionLine{
				&complFeed.ActionLine{
					Action: "update",
					Slots:  make([]*complFeed.Timeslot, 4),
				},
				&complFeed.ActionLine{
					Action: "create",
					Slots:  make([]*complFeed.Timeslot, 4),
				},
				&complFeed.ActionLine{
					Action: "delete",
					Slots:  make([]*complFeed.Timeslot, 4),
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
				&cmsRes.EventString{
					EventAction: "update",
					Collection: []*cmsRes.EventCollection{
						&cmsRes.EventCollection{}, &cmsRes.EventCollection{
							EventsCount: []*cmsRes.EventCount{
								&cmsRes.EventCount{Name: "node", Count: 1},
							},
						},
						&cmsRes.EventCollection{}, &cmsRes.EventCollection{},
					},
				},
				&cmsRes.EventString{
					EventAction: "create",
					Collection: []*cmsRes.EventCollection{
						&cmsRes.EventCollection{}, &cmsRes.EventCollection{},
						&cmsRes.EventCollection{
							EventsCount: []*cmsRes.EventCount{
								&cmsRes.EventCount{Name: "cookbook", Count: 1},
							},
						}, &cmsRes.EventCollection{},
					},
				},
				&cmsRes.EventString{
					EventAction: "delete",
					Collection: []*cmsRes.EventCollection{
						&cmsRes.EventCollection{}, &cmsRes.EventCollection{
							EventsCount: []*cmsRes.EventCount{
								&cmsRes.EventCount{Name: "item", Count: 1},
							},
						},
						&cmsRes.EventCollection{}, &cmsRes.EventCollection{
							EventsCount: []*cmsRes.EventCount{
								&cmsRes.EventCount{Name: "bag", Count: 1},
							},
						},
					},
				},
			},
		}, nil
	})

	mockFeedServiceClient := mock_automate_feed.NewMockFeedServiceClient(ctrl)
	mockFeedServiceClient.EXPECT().GetFeedTimeline(
		context.Background(),
		gomock.Any(),
	).DoAndReturn(func(c context.Context, request *complFeed.FeedTimelineRequest) (*complFeed.FeedTimelineResponse, error) {
		return &complFeed.FeedTimelineResponse{
			Start:    request.Start,
			End:      request.End,
			Interval: request.Interval,
			ActionLines: []*complFeed.ActionLine{
				&complFeed.ActionLine{
					Action: "update",
					Slots: []*complFeed.Timeslot{
						&complFeed.Timeslot{
							Counts: []*complFeed.EntryCount{
								&complFeed.EntryCount{Category: "scanjobs", Count: 1},
							},
						},
						&complFeed.Timeslot{}, &complFeed.Timeslot{}, &complFeed.Timeslot{},
					},
				},
				&complFeed.ActionLine{
					Action: "create",
					Slots: []*complFeed.Timeslot{
						&complFeed.Timeslot{}, &complFeed.Timeslot{}, &complFeed.Timeslot{},
						&complFeed.Timeslot{
							Counts: []*complFeed.EntryCount{
								&complFeed.EntryCount{Category: "profile", Count: 1},
							},
						},
					},
				},
				&complFeed.ActionLine{
					Action: "delete",
					Slots: []*complFeed.Timeslot{
						&complFeed.Timeslot{}, &complFeed.Timeslot{},
						&complFeed.Timeslot{
							Counts: []*complFeed.EntryCount{
								&complFeed.EntryCount{Category: "scanjobs", Count: 1},
							},
						}, &complFeed.Timeslot{},
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
				&cmsRes.EventString{
					EventAction: "update",
					Collection: []*cmsRes.EventCollection{
						&cmsRes.EventCollection{
							EventsCount: []*cmsRes.EventCount{
								&cmsRes.EventCount{Name: "node", Count: 5},
							},
						}, &cmsRes.EventCollection{}, &cmsRes.EventCollection{}, &cmsRes.EventCollection{},
					},
				},
				&cmsRes.EventString{
					EventAction: "create",
					Collection: []*cmsRes.EventCollection{
						&cmsRes.EventCollection{}, &cmsRes.EventCollection{}, &cmsRes.EventCollection{},
						&cmsRes.EventCollection{
							EventsCount: []*cmsRes.EventCount{
								&cmsRes.EventCount{Name: "cookbook", Count: 100},
							},
						},
					},
				},
				&cmsRes.EventString{
					EventAction: "delete",
					Collection: []*cmsRes.EventCollection{
						&cmsRes.EventCollection{}, &cmsRes.EventCollection{},
						&cmsRes.EventCollection{
							EventsCount: []*cmsRes.EventCount{
								&cmsRes.EventCount{Name: "environment", Count: 999},
								&cmsRes.EventCount{Name: "role", Count: 450},
								&cmsRes.EventCount{Name: "cookbook", Count: 20},
								&cmsRes.EventCount{Name: "bag", Count: 5},
								&cmsRes.EventCount{Name: "scanjobs", Count: 100},
							},
						}, &cmsRes.EventCollection{},
					},
				},
			},
		}, nil
	})

	mockFeedServiceClient := mock_automate_feed.NewMockFeedServiceClient(ctrl)
	mockFeedServiceClient.EXPECT().GetFeedTimeline(
		context.Background(),
		gomock.Any(),
	).DoAndReturn(func(c context.Context, request *complFeed.FeedTimelineRequest) (*complFeed.FeedTimelineResponse, error) {
		return &complFeed.FeedTimelineResponse{
			Start:    request.Start,
			End:      request.End,
			Interval: request.Interval,
			ActionLines: []*complFeed.ActionLine{
				&complFeed.ActionLine{
					Action: "update",
					Slots: []*complFeed.Timeslot{
						&complFeed.Timeslot{
							Counts: []*complFeed.EntryCount{
								&complFeed.EntryCount{Category: "scanjobs", Count: 10},
							},
						}, &complFeed.Timeslot{}, &complFeed.Timeslot{}, &complFeed.Timeslot{},
					},
				},
				&complFeed.ActionLine{
					Action: "create",
					Slots: []*complFeed.Timeslot{
						&complFeed.Timeslot{}, &complFeed.Timeslot{}, &complFeed.Timeslot{},
						&complFeed.Timeslot{
							Counts: []*complFeed.EntryCount{
								&complFeed.EntryCount{Category: "profile", Count: 50},
							},
						},
					},
				},
				&complFeed.ActionLine{
					Action: "delete",
					Slots: []*complFeed.Timeslot{
						&complFeed.Timeslot{}, &complFeed.Timeslot{},
						&complFeed.Timeslot{
							Counts: []*complFeed.EntryCount{
								&complFeed.EntryCount{Category: "scanjobs", Count: 500},
								&complFeed.EntryCount{Category: "profile", Count: 100},
							},
						}, &complFeed.Timeslot{},
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
				&cmsRes.EventString{
					EventAction: "update",
					Collection: []*cmsRes.EventCollection{
						&cmsRes.EventCollection{}, &cmsRes.EventCollection{}, &cmsRes.EventCollection{}, &cmsRes.EventCollection{},
					},
				},
				&cmsRes.EventString{
					EventAction: "create",
					Collection: []*cmsRes.EventCollection{
						&cmsRes.EventCollection{}, &cmsRes.EventCollection{}, &cmsRes.EventCollection{},
						&cmsRes.EventCollection{},
					},
				},
				&cmsRes.EventString{
					EventAction: "delete",
					Collection: []*cmsRes.EventCollection{
						&cmsRes.EventCollection{}, &cmsRes.EventCollection{},
						&cmsRes.EventCollection{
							EventsCount: []*cmsRes.EventCount{
								&cmsRes.EventCount{Name: "profile", Count: 1},
							},
						}, &cmsRes.EventCollection{},
					},
				},
			},
		}, nil
	})

	mockFeedServiceClient := mock_automate_feed.NewMockFeedServiceClient(ctrl)
	mockFeedServiceClient.EXPECT().GetFeedTimeline(
		context.Background(),
		gomock.Any(),
	).DoAndReturn(func(c context.Context, request *complFeed.FeedTimelineRequest) (*complFeed.FeedTimelineResponse, error) {
		return &complFeed.FeedTimelineResponse{}, status.Error(codes.Unavailable, "compliance service not running")
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
				&cmsRes.EventString{
					EventAction: "update",
					Collection:  make([]*cmsRes.EventCollection, 4),
				},
				// &cmsRes.EventString{
				// 	EventAction: "create",   // the missing string
				//  Collection:  make([]*cmsRes.EventCollection, 4),
				// },
				&cmsRes.EventString{
					EventAction: "delete",
					Collection:  make([]*cmsRes.EventCollection, 4),
				},
			},
		}, nil
	})

	mockFeedServiceClient := mock_automate_feed.NewMockFeedServiceClient(ctrl)
	mockFeedServiceClient.EXPECT().GetFeedTimeline(
		context.Background(),
		gomock.Any(),
	).DoAndReturn(func(c context.Context, request *complFeed.FeedTimelineRequest) (*complFeed.FeedTimelineResponse, error) {
		return &complFeed.FeedTimelineResponse{
			Start:    request.Start,
			End:      request.End,
			Interval: request.Interval,
			ActionLines: []*complFeed.ActionLine{
				&complFeed.ActionLine{
					Action: "update",
					Slots:  make([]*complFeed.Timeslot, 4),
				},
				&complFeed.ActionLine{
					Action: "create",
					Slots:  make([]*complFeed.Timeslot, 4),
				},
				&complFeed.ActionLine{
					Action: "delete",
					Slots:  make([]*complFeed.Timeslot, 4),
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
				&cmsRes.EventString{
					EventAction: "update",
					Collection:  make([]*cmsRes.EventCollection, 4),
				},
				&cmsRes.EventString{
					EventAction: "update", // the Duplicate string
					Collection:  make([]*cmsRes.EventCollection, 4),
				},
				&cmsRes.EventString{
					EventAction: "delete",
					Collection:  make([]*cmsRes.EventCollection, 4),
				},
			},
		}, nil
	})

	mockFeedServiceClient := mock_automate_feed.NewMockFeedServiceClient(ctrl)
	mockFeedServiceClient.EXPECT().GetFeedTimeline(
		context.Background(),
		gomock.Any(),
	).DoAndReturn(func(c context.Context, request *complFeed.FeedTimelineRequest) (*complFeed.FeedTimelineResponse, error) {
		return &complFeed.FeedTimelineResponse{
			Start:    request.Start,
			End:      request.End,
			Interval: request.Interval,
			ActionLines: []*complFeed.ActionLine{
				&complFeed.ActionLine{
					Action: "update",
					Slots:  make([]*complFeed.Timeslot, 4),
				},
				&complFeed.ActionLine{
					Action: "create",
					Slots:  make([]*complFeed.Timeslot, 4),
				},
				&complFeed.ActionLine{
					Action: "delete",
					Slots:  make([]*complFeed.Timeslot, 4),
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
				&cmsRes.EventString{
					EventAction: "update",
					Collection:  make([]*cmsRes.EventCollection, 4),
				},
				&cmsRes.EventString{
					EventAction: "create",
					Collection:  make([]*cmsRes.EventCollection, 4),
				},
				&cmsRes.EventString{
					EventAction: "delete",
					Collection:  make([]*cmsRes.EventCollection, 4),
				},
			},
		}, nil
	})

	mockFeedServiceClient := mock_automate_feed.NewMockFeedServiceClient(ctrl)
	mockFeedServiceClient.EXPECT().GetFeedTimeline(
		context.Background(),
		gomock.Any(),
	).DoAndReturn(func(c context.Context, request *complFeed.FeedTimelineRequest) (*complFeed.FeedTimelineResponse, error) {
		return &complFeed.FeedTimelineResponse{
			Start:    request.Start,
			End:      request.End,
			Interval: request.Interval,
			ActionLines: []*complFeed.ActionLine{
				&complFeed.ActionLine{
					Action: "update",
					Slots:  make([]*complFeed.Timeslot, 4),
				},
				&complFeed.ActionLine{
					Action: "update", // the Duplicate string
					Slots:  make([]*complFeed.Timeslot, 4),
				},
				&complFeed.ActionLine{
					Action: "delete",
					Slots:  make([]*complFeed.Timeslot, 4),
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

	mockFeedServiceClient := mock_automate_feed.NewMockFeedServiceClient(ctrl)
	mockFeedServiceClient.EXPECT().GetFeedTimeline(
		context.Background(),
		gomock.Any(),
	).DoAndReturn(func(c context.Context, request *complFeed.FeedTimelineRequest) (*complFeed.FeedTimelineResponse, error) {
		return &complFeed.FeedTimelineResponse{
			Start:    request.Start,
			End:      request.End,
			Interval: request.Interval,
			ActionLines: []*complFeed.ActionLine{
				&complFeed.ActionLine{
					Action: "update",
					Slots: []*complFeed.Timeslot{
						&complFeed.Timeslot{
							Counts: []*complFeed.EntryCount{
								&complFeed.EntryCount{Category: "scanjobs", Count: 1},
							},
						},
						&complFeed.Timeslot{}, &complFeed.Timeslot{},
						&complFeed.Timeslot{},
					},
				},
				&complFeed.ActionLine{
					Action: "create",
					Slots: []*complFeed.Timeslot{
						&complFeed.Timeslot{}, &complFeed.Timeslot{},
						&complFeed.Timeslot{
							Counts: []*complFeed.EntryCount{
								&complFeed.EntryCount{Category: "scanjobs", Count: 2},
							},
						},
						&complFeed.Timeslot{},
					},
				},
				&complFeed.ActionLine{
					Action: "delete",
					Slots: []*complFeed.Timeslot{
						&complFeed.Timeslot{}, &complFeed.Timeslot{},
						&complFeed.Timeslot{}, &complFeed.Timeslot{
							Counts: []*complFeed.EntryCount{
								&complFeed.EntryCount{Category: "scanjobs", Count: 3},
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
				&cmsRes.EventString{
					EventAction: "update",
					Collection: []*cmsRes.EventCollection{
						&cmsRes.EventCollection{
							EventsCount: []*cmsRes.EventCount{
								&cmsRes.EventCount{Name: "bag", Count: 1},
							},
						},
						&cmsRes.EventCollection{}, &cmsRes.EventCollection{},
						&cmsRes.EventCollection{},
					},
				},
				&cmsRes.EventString{
					EventAction: "create",
					Collection: []*cmsRes.EventCollection{
						&cmsRes.EventCollection{}, &cmsRes.EventCollection{},
						&cmsRes.EventCollection{
							EventsCount: []*cmsRes.EventCount{
								&cmsRes.EventCount{Name: "cookbook", Count: 2},
							},
						},
						&cmsRes.EventCollection{},
					},
				},
				&cmsRes.EventString{
					EventAction: "delete",
					Collection: []*cmsRes.EventCollection{
						&cmsRes.EventCollection{}, &cmsRes.EventCollection{},
						&cmsRes.EventCollection{}, &cmsRes.EventCollection{
							EventsCount: []*cmsRes.EventCount{
								&cmsRes.EventCount{Name: "node", Count: 3},
							},
						},
					},
				},
			},
		}, nil
	})

	mockFeedServiceClient := mock_automate_feed.NewMockFeedServiceClient(ctrl)
	mockFeedServiceClient.EXPECT().GetFeedTimeline(
		context.Background(),
		gomock.Any(),
	).DoAndReturn(func(c context.Context, request *complFeed.FeedTimelineRequest) (*complFeed.FeedTimelineResponse, error) {
		return &complFeed.FeedTimelineResponse{}, status.Error(codes.Unavailable, "compliance service not running")
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

	mockFeedServiceClient := mock_automate_feed.NewMockFeedServiceClient(ctrl)
	mockFeedServiceClient.EXPECT().GetFeedTimeline(
		context.Background(),
		gomock.Any(),
	).DoAndReturn(func(c context.Context, request *complFeed.FeedTimelineRequest) (*complFeed.FeedTimelineResponse, error) {
		return &complFeed.FeedTimelineResponse{}, status.Error(codes.Unavailable, "compliance service not running")
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
	mockFeedServiceClient := mock_automate_feed.NewMockFeedServiceClient(ctrl)

	eventFeedAggregate := subject.NewEventFeedAggregate(mockCfgMgmtClient, mockFeedServiceClient)

	cases := []*agReq.EventStrings{
		&agReq.EventStrings{
			Start:        "2018-01-01",
			End:          "2018-01-06",
			HoursBetween: 3,
			Timezone:     "",
		},
		&agReq.EventStrings{
			Start:        "2018-01-01",
			End:          "2018-01-06",
			HoursBetween: 3,
			Timezone:     "fake",
		},
		&agReq.EventStrings{
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
	mockFeedServiceClient := mock_automate_feed.NewMockFeedServiceClient(ctrl)

	eventFeedAggregate := subject.NewEventFeedAggregate(mockCfgMgmtClient, mockFeedServiceClient)

	cases := []agReq.EventStrings{
		agReq.EventStrings{
			Start:        "2018-01-06",
			End:          "2018-01-01",
			HoursBetween: 3,
			Timezone:     "UTC",
		},
		agReq.EventStrings{
			Start:        "2000-00-00",
			End:          "2000-00-06",
			HoursBetween: 3,
			Timezone:     "UTC",
		},
		agReq.EventStrings{
			Start:        "00-00-00",
			End:          "00-00-06",
			HoursBetween: 3,
			Timezone:     "UTC",
		},
		agReq.EventStrings{
			Start:        "18-10-10",
			End:          "18-10-16",
			HoursBetween: 3,
			Timezone:     "UTC",
		},
		agReq.EventStrings{
			Start:        "20-01-01",
			End:          "20-01-06",
			HoursBetween: 3,
			Timezone:     "UTC",
		},
		agReq.EventStrings{
			Start:        "17:01:01",
			End:          "17:01:06",
			HoursBetween: 3,
			Timezone:     "UTC",
		},
		agReq.EventStrings{
			Start:        "01-01-2000",
			End:          "06-01-2000",
			HoursBetween: 3,
			Timezone:     "UTC",
		},
		agReq.EventStrings{
			Start:        "3000-12",
			End:          "3006-12",
			HoursBetween: 3,
			Timezone:     "UTC",
		},
		agReq.EventStrings{
			Start:        "2019",
			End:          "2018",
			HoursBetween: 3,
			Timezone:     "UTC",
		},
		agReq.EventStrings{
			Start:        "1888:01:01",
			End:          "1888:01:06",
			HoursBetween: 3,
			Timezone:     "UTC",
		},
		agReq.EventStrings{
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
	mockFeedServiceClient := mock_automate_feed.NewMockFeedServiceClient(ctrl)

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
