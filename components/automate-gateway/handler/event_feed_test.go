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
	agReq "github.com/chef/automate/components/automate-gateway/api/event_feed/request"
	agRes "github.com/chef/automate/components/automate-gateway/api/event_feed/response"
	mock_automate_feed "github.com/chef/automate/components/automate-gateway/gateway_mocks/mock_feed"
	subject "github.com/chef/automate/components/automate-gateway/handler"
	automate_feed "github.com/chef/automate/components/compliance-service/api/automate-feed"
	complFeed "github.com/chef/automate/components/compliance-service/api/automate-feed"
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

	mockFeedServiceClient := mock_automate_feed.NewMockFeedServiceClient(ctrl)
	mockFeedServiceClient.EXPECT().GetFeed(
		c,
		gomock.Any(),
	).DoAndReturn(func(c ctx.Context, action *automate_feed.FeedRequest) (*automate_feed.FeedResponse, error) {
		return &automate_feed.FeedResponse{}, nil
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
		mockFeedServiceClient = mock_automate_feed.NewMockFeedServiceClient(ctrl)
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
	).DoAndReturn(func(c ctx.Context, request *automate_feed.FeedRequest) (*automate_feed.FeedResponse, error) {
		return &automate_feed.FeedResponse{}, nil
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
		mockFeedServiceClient = mock_automate_feed.NewMockFeedServiceClient(ctrl)
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
	).DoAndReturn(func(c ctx.Context, action *automate_feed.FeedRequest) (*automate_feed.FeedResponse, error) {
		return &automate_feed.FeedResponse{}, errors.New("Oh no! Something also went wrong here.")
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

	mockFeedServiceClient := mock_automate_feed.NewMockFeedServiceClient(ctrl)
	mockFeedServiceClient.EXPECT().GetFeedTimeline(
		c,
		gomock.Any(),
	).DoAndReturn(func(c ctx.Context, action *complFeed.FeedTimelineRequest) (*complFeed.FeedTimelineResponse, error) {
		return &complFeed.FeedTimelineResponse{
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
		&cmsRes.EventCollection{
			EventsCount: []*cmsRes.EventCount{
				&cmsRes.EventCount{Name: "a"},
			},
		},
		&cmsRes.EventCollection{
			EventsCount: []*cmsRes.EventCount{
				&cmsRes.EventCount{Name: "b"},
			},
		},
	}

	var cmsStringSlice = []*cmsRes.EventString{
		&cmsRes.EventString{
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

	var feedActionLine = []*complFeed.ActionLine{
		&complFeed.ActionLine{
			Slots:  make([]*complFeed.Timeslot, 2),
			Action: "create",
		},
	}

	mockFeedServiceClient := mock_automate_feed.NewMockFeedServiceClient(ctrl)
	mockFeedServiceClient.EXPECT().GetFeedTimeline(
		c,
		gomock.Any(),
	).DoAndReturn(func(c ctx.Context, action *complFeed.FeedTimelineRequest) (*complFeed.FeedTimelineResponse, error) {
		return &complFeed.FeedTimelineResponse{
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
			&agRes.EventCollection{
				EventsCount: []*agRes.EventCount{
					&agRes.EventCount{Name: "a"},
				},
			},
			&agRes.EventCollection{
				EventsCount: []*agRes.EventCount{
					&agRes.EventCount{Name: "b"},
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
