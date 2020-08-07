//
//  Author:: Salim Afiune <afiune@chef.io>
//  Copyright:: Copyright 2018, Chef Software Inc.
//

package handler_test

import (
	ctx "context"
	"errors"
	"testing"
	"time"

	cmsReq "github.com/chef/automate/api/interservice/cfgmgmt/request"
	cmsRes "github.com/chef/automate/api/interservice/cfgmgmt/response"
	cmsService "github.com/chef/automate/api/interservice/cfgmgmt/service"
	event_feed_api "github.com/chef/automate/api/interservice/event_feed"
	agReq "github.com/chef/automate/components/automate-gateway/api/event_feed/request"
	agRes "github.com/chef/automate/components/automate-gateway/api/event_feed/response"
	subject "github.com/chef/automate/components/automate-gateway/handler"
	"github.com/golang/mock/gomock"
	"github.com/stretchr/testify/assert"
)

// TestEventFeedFuncGetEventFeedEmptyRequest a very simple mock with empty request/response
func TestEventFeedFuncGetEventFeedEmptyRequest(t *testing.T) {
	ctrl := gomock.NewController(t)
	mockCfgMgmtClient := cmsService.NewMockCfgMgmtClient(ctrl)

	c := ctx.Background()
	mockCfgMgmtClient.EXPECT().GetEventFeed(
		c,
		gomock.Any(),
	).DoAndReturn(func(c ctx.Context, action *cmsReq.EventFilter) (*cmsRes.Events, error) {
		return &cmsRes.Events{}, nil
	})

	mockFeedServiceClient := event_feed_api.NewMockEventFeedServiceClient(ctrl)
	mockFeedServiceClient.EXPECT().GetFeed(
		c,
		gomock.Any(),
	).DoAndReturn(func(c ctx.Context, action *event_feed_api.FeedRequest) (*event_feed_api.FeedResponse, error) {
		return &event_feed_api.FeedResponse{}, nil
	})

	eventFeedServer := subject.NewEventFeedServer(mockCfgMgmtClient, mockFeedServiceClient)

	request := &agReq.EventFilter{
		PageSize: 1000,
	}
	events, err := eventFeedServer.GetEventFeed(c, request)

	assert.Nil(t, err)
	assert.Equal(t, 0, len(events.Events))
}

// TestEventFeedFuncGetEventFeedSingleErrorShouldNotFail test when config-mgmt service fails
// it should still work, as well as when the compliance service fails
func TestEventFeedFuncGetEventFeedSingleErrorShouldNotFail(t *testing.T) {
	var (
		ctrl                  = gomock.NewController(t)
		c                     = ctx.Background()
		mockCfgMgmtClient     = cmsService.NewMockCfgMgmtClient(ctrl)
		mockFeedServiceClient = event_feed_api.NewMockEventFeedServiceClient(ctrl)
		request               = &agReq.EventFilter{
			PageSize: 1000,
		}
	)

	// Lets mock a failure coming from the config-mgmt-service
	mockCfgMgmtClient.EXPECT().GetEventFeed(
		c,
		gomock.Any(),
	).DoAndReturn(func(c ctx.Context, action *cmsReq.EventFilter) (*cmsRes.Events, error) {
		return &cmsRes.Events{}, errors.New("Oh no! Something went wrong here.")
	})

	mockFeedServiceClient.EXPECT().GetFeed(
		ctx.Background(),
		gomock.Any(),
	).DoAndReturn(func(c ctx.Context, request *event_feed_api.FeedRequest) (*event_feed_api.FeedResponse, error) {
		return &event_feed_api.FeedResponse{}, nil
	})

	eventFeedServer := subject.NewEventFeedServer(mockCfgMgmtClient, mockFeedServiceClient)
	events, err := eventFeedServer.GetEventFeed(c, request)
	assert.Nil(t, err)
	assert.Equal(t, 0, len(events.Events))
}

// TestEventFeedFuncGetEventFeedAllSubscribersFailedThenExpectFailure if every single service
// failed to collect the feed of events, expect a global failure
func TestEventFeedFuncGetEventFeedAllSubscribersFailedThenExpectFailure(t *testing.T) {
	var (
		ctrl                  = gomock.NewController(t)
		c                     = ctx.Background()
		mockCfgMgmtClient     = cmsService.NewMockCfgMgmtClient(ctrl)
		mockFeedServiceClient = event_feed_api.NewMockEventFeedServiceClient(ctrl)
		request               = &agReq.EventFilter{
			PageSize: 1000,
		}
	)

	// Lets mock a failure coming from the config-mgmt-service
	mockCfgMgmtClient.EXPECT().GetEventFeed(
		c,
		gomock.Any(),
	).DoAndReturn(func(c ctx.Context, action *cmsReq.EventFilter) (*cmsRes.Events, error) {
		return &cmsRes.Events{}, errors.New("Oh no! Something went wrong here.")
	})

	// Lets mock a failure coming from the config-mgmt-service
	/*mockComplianceEventsClient.EXPECT().GetEventFeed(
		c,
		gomock.Any(),
	).DoAndReturn(func(c ctx.Context, action *cmplEvents.EventFilterRequest) (*cmplEvents.EventsResponse, error) {
		return &cmplEvents.EventsResponse{}, errors.New("Oh no! Something also went wrong here.")
	})*/

	// mock failure coming from feed service
	mockFeedServiceClient.EXPECT().GetFeed(
		ctx.Background(),
		gomock.Any(),
	).DoAndReturn(func(c ctx.Context, action *event_feed_api.FeedRequest) (*event_feed_api.FeedResponse, error) {
		return &event_feed_api.FeedResponse{}, errors.New("Oh no! Something also went wrong here.")
	})

	eventFeedServer := subject.NewEventFeedServer(mockCfgMgmtClient, mockFeedServiceClient)
	events, err := eventFeedServer.GetEventFeed(c, request)

	// We should FAIL since all our services failed
	assert.NotNil(t, err)

	// The error should contain both errors
	assert.Contains(t, err.Error(), "Oh no! Something went wrong here.")
	assert.Contains(t, err.Error(), "Oh no! Something also went wrong here.")
	assert.Equal(t, 0, len(events.Events))
}

// TestEventStringsFuncGetEventStringBucketsEmptyRequest sending empty request/response
func TestEventStringsFuncGetEventStringBucketsEmptyRequest(t *testing.T) {
	timezone := "America/Los_Angeles"
	loc, _ := time.LoadLocation(timezone)
	startDate := time.Now().In(loc)
	ctrl := gomock.NewController(t)
	hoursBetween := int32(3)
	mockCfgMgmtClient := cmsService.NewMockCfgMgmtClient(ctrl)

	c := ctx.Background()
	mockCfgMgmtClient.EXPECT().GetEventStringBuckets(
		c,
		gomock.Any(),
	).DoAndReturn(func(c ctx.Context, action *cmsReq.EventStrings) (*cmsRes.EventStrings, error) {
		return &cmsRes.EventStrings{
			HoursBetween: hoursBetween,
			Start:        startDate.AddDate(0, 0, -6).Format("2006-01-02"),
			End:          startDate.Format("2006-01-02"),
		}, nil
	})

	mockFeedServiceClient := event_feed_api.NewMockEventFeedServiceClient(ctrl)
	mockFeedServiceClient.EXPECT().GetFeedTimeline(
		c,
		gomock.Any(),
	).DoAndReturn(func(c ctx.Context, action *event_feed_api.FeedTimelineRequest) (*event_feed_api.FeedTimelineResponse, error) {
		return &event_feed_api.FeedTimelineResponse{
			Interval: hoursBetween,
			Start:    startDate.AddDate(0, 0, -6).Format("2006-01-02"),
			End:      startDate.Format("2006-01-02"),
		}, nil
	})

	eventFeedServer := subject.NewEventFeedServer(mockCfgMgmtClient, mockFeedServiceClient)

	request := &agReq.EventStrings{
		HoursBetween: hoursBetween,
		Start:        startDate.AddDate(0, 0, -6).Format("2006-01-02"),
		End:          startDate.Format("2006-01-02"),
		Timezone:     timezone,
	}
	strings, err := eventFeedServer.GetEventStringBuckets(c, request)

	assert.Nil(t, err)
	assert.Equal(t, 0, len(strings.Strings))
}

// TestEventStringsFuncGetEventStringBucketsCorrectRequest sends correct response
func TestEventStringsFuncGetEventStringBucketsCorrectRequest(t *testing.T) {
	ctrl := gomock.NewController(t)
	mockCfgMgmtClient := cmsService.NewMockCfgMgmtClient(ctrl)

	c := ctx.Background()

	functionCalled := false

	collection := []*cmsRes.EventCollection{
		{
			EventsCount: []*cmsRes.EventCount{
				{Name: "a"},
			},
		},
		{
			EventsCount: []*cmsRes.EventCount{
				{Name: "b"},
			},
		},
	}

	var cmsStringSlice = []*cmsRes.EventString{
		{
			Collection:  collection,
			EventAction: "create",
		},
	}

	mockCfgMgmtClient.EXPECT().GetEventStringBuckets(
		c,
		gomock.Any(),
	).DoAndReturn(func(c ctx.Context, action *cmsReq.EventStrings) (*cmsRes.EventStrings, error) {
		functionCalled = true
		return &cmsRes.EventStrings{
			Strings:      cmsStringSlice,
			Start:        "2018-02-01",
			End:          "2018-02-02",
			HoursBetween: 24,
		}, nil
	})

	var feedActionLine = []*event_feed_api.ActionLine{
		{
			Slots:  make([]*event_feed_api.Timeslot, 2),
			Action: "create",
		},
	}

	mockFeedServiceClient := event_feed_api.NewMockEventFeedServiceClient(ctrl)
	mockFeedServiceClient.EXPECT().GetFeedTimeline(
		c,
		gomock.Any(),
	).DoAndReturn(func(c ctx.Context, action *event_feed_api.FeedTimelineRequest) (*event_feed_api.FeedTimelineResponse, error) {
		return &event_feed_api.FeedTimelineResponse{
			ActionLines: feedActionLine,
			Start:       "2018-02-01",
			End:         "2018-02-02",
			Interval:    24,
		}, nil
	})

	eventFeedServer := subject.NewEventFeedServer(mockCfgMgmtClient, mockFeedServiceClient)

	request := &agReq.EventStrings{
		Start:        "2018-02-01",
		End:          "2018-02-02",
		Timezone:     "America/Los_Angeles",
		HoursBetween: 24,
	}

	expectedEventString := []*agRes.EventString{}
	agInsides := &agRes.EventString{
		Collection: []*agRes.EventCollection{
			{
				EventsCount: []*agRes.EventCount{
					{Name: "a"},
				},
			},
			{
				EventsCount: []*agRes.EventCount{
					{Name: "b"},
				},
			},
		},
		EventAction: "create",
	}
	expectedEventString = append(expectedEventString, agInsides)

	expectedResponse := &agRes.EventStrings{
		Strings:      expectedEventString,
		Start:        "2018-02-01",
		End:          "2018-02-02",
		HoursBetween: 24,
	}

	strings, err := eventFeedServer.GetEventStringBuckets(c, request)

	assert.Nil(t, err)
	assert.Equal(t, 1, len(strings.Strings))
	assert.Equal(t, strings, expectedResponse)
	assert.True(t, functionCalled)
}
