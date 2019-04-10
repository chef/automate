package eventfeed_test

import (
	"context"
	"fmt"
	"sort"
	"strconv"
	"testing"
	"time"

	cmsReq "github.com/chef/automate/api/interservice/cfgmgmt/request"
	cmsRes "github.com/chef/automate/api/interservice/cfgmgmt/response"
	agReq "github.com/chef/automate/components/automate-gateway/api/event_feed/request"
	subject "github.com/chef/automate/components/automate-gateway/eventfeed"
	"github.com/chef/automate/components/automate-gateway/gateway_mocks/mock_cfgmgmt"
	mock_automate_feed "github.com/chef/automate/components/automate-gateway/gateway_mocks/mock_feed"
	automate_feed "github.com/chef/automate/components/compliance-service/api/automate-feed"
	"github.com/golang/mock/gomock"
	"github.com/golang/protobuf/ptypes"
	"github.com/stretchr/testify/assert"
	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/status"
)

func TestEventFeedEmpty(t *testing.T) {
	ctrl := gomock.NewController(t)

	mockCfgMgmtClient := mock_cfgmgmt.NewMockCfgMgmtClient(ctrl)
	mockCfgMgmtClient.EXPECT().GetEventFeed(
		context.Background(),
		gomock.Any(),
	).DoAndReturn(func(c context.Context, action *cmsReq.EventFilter) (*cmsRes.Events, error) {
		return &cmsRes.Events{}, nil
	})

	mockFeedServiceClient := mock_automate_feed.NewMockFeedServiceClient(ctrl)
	mockFeedServiceClient.EXPECT().GetFeed(
		context.Background(),
		gomock.Any(),
	).DoAndReturn(func(c context.Context, action *automate_feed.FeedRequest) (*automate_feed.FeedResponse, error) {
		return &automate_feed.FeedResponse{}, nil
	})

	eventFeedAggregate := subject.NewEventFeedAggregate(mockCfgMgmtClient, mockFeedServiceClient)

	eventCounts, err := eventFeedAggregate.CollectEventFeed(
		context.Background(),
		&agReq.EventFilter{
			PageSize: 1000,
		},
	)

	assert.Nil(t, err)
	assert.Equal(t, int64(0), eventCounts.TotalEvents,
		"Total number of events within range, should be zero")
}

// TestEventFeedCollectEventFeedCollapseFalse Sending collapse=false (default)
func TestEventFeedCollectEventFeedCollapseFalse(t *testing.T) {
	ctrl := gomock.NewController(t)

	mockCfgMgmtClient := mock_cfgmgmt.NewMockCfgMgmtClient(ctrl)
	mockCfgMgmtClient.EXPECT().GetEventFeed(
		context.Background(),
		gomock.Any(),
	).DoAndReturn(func(c context.Context, action *cmsReq.EventFilter) (*cmsRes.Events, error) {
		// Here is where we mock the Config Mgmt Service
		// Lets return a fixed set of actions
		return cmsFixedEvents(t), nil
	})

	mockFeedServiceClient := mock_automate_feed.NewMockFeedServiceClient(ctrl)
	mockFeedServiceClient.EXPECT().GetFeed(
		context.Background(),
		gomock.Any(),
	).DoAndReturn(func(c context.Context, action *automate_feed.FeedRequest) (*automate_feed.FeedResponse, error) {
		return complianceEventFixedEvents(t), nil
	})

	eventFeedAggregate := subject.NewEventFeedAggregate(mockCfgMgmtClient, mockFeedServiceClient)
	events, err := eventFeedAggregate.CollectEventFeed(context.Background(),
		&agReq.EventFilter{
			Collapse: false,
			PageSize: 1000,
		},
	)

	assert.Nil(t, err)
	// We should return all 202 events, we are NOT collapsing
	assert.Equal(t, 202, len(events.Events))
}

// TestEventFeedCollectEventFeedCollapseTrue Sending collapse=true
func TestEventFeedCollectEventFeedCollapseNonoverlapping(t *testing.T) {
	// Enable event grouping
	request := &agReq.EventFilter{Collapse: true, PageSize: 1000}

	mockCfgMgmtClient := mock_cfgmgmt.NewMockCfgMgmtClient(gomock.NewController(t))
	mockCfgMgmtClient.EXPECT().GetEventFeed(
		context.Background(),
		gomock.Any(),
	).DoAndReturn(func(c context.Context, action *cmsReq.EventFilter) (*cmsRes.Events, error) {
		// Here is where we mock the Config Mgmt Service
		// Lets return a fixed set of actions
		return cmsFixedEvents(t), nil
	})

	mockFeedServiceClient := mock_automate_feed.NewMockFeedServiceClient(gomock.NewController(t))
	mockFeedServiceClient.EXPECT().GetFeed(
		context.Background(),
		gomock.Any(),
	).DoAndReturn(func(c context.Context, action *automate_feed.FeedRequest) (*automate_feed.FeedResponse, error) {
		return complianceEventFixedEvents(t), nil
	})

	eventFeedAggregate := subject.NewEventFeedAggregate(mockCfgMgmtClient, mockFeedServiceClient)
	events, err := eventFeedAggregate.CollectEventFeed(context.Background(), request)

	assert.Nil(t, err)
	// We should return only 4 events since we ARE collapsing
	assert.Equal(t, 4, len(events.Events))
	assert.Equal(t, "2018-01-30T21:20:41Z", ptypes.TimestampString(events.Events[1].EndTime))
	assert.Equal(t, "2018-01-20T21:20:41Z", ptypes.TimestampString(events.Events[3].EndTime))
}

// TestEventFeedCollectEventFeedCollapseOverlapping - test that events from compliance
// break up grouping of events from config-mgmt. We have two events from config-mgmt at the
// same time, then two events from compliance a second after. Then two more config-mgmt events a second later
// A total of 6 events that should be grouped into 3 items.
func TestEventFeedCollectEventFeedCollapseOverlapping(t *testing.T) {
	request := &agReq.EventFilter{Collapse: true, PageSize: 1000}

	time1 := time.Now()
	timestamp1, err := ptypes.TimestampProto(time1)
	assert.Nil(t, err)

	time2 := time1.Add(time.Second * -1)
	timestamp2, err := ptypes.TimestampProto(time2)
	assert.Nil(t, err)

	time3 := time2.Add(time.Second * -1)
	timestamp3, err := ptypes.TimestampProto(time3)
	assert.Nil(t, err)

	mockCfgMgmtClient := mock_cfgmgmt.NewMockCfgMgmtClient(gomock.NewController(t))
	mockCfgMgmtClient.EXPECT().GetEventFeed(
		context.Background(),
		gomock.Any(),
	).DoAndReturn(func(c context.Context, action *cmsReq.EventFilter) (*cmsRes.Events, error) {
		fixedEvents := make([]*cmsRes.Event, 0)

		for count := 0; count < 5; count++ {
			fixedEvents = append(fixedEvents, &cmsRes.Event{
				EventType:       "cookbook",
				Task:            "update",
				Timestamp:       timestamp1,
				EntityName:      "mocked_event",
				RequestorType:   "automatic",
				RequestorName:   "golang",
				ServiceHostname: "localhost",
			})
		}

		for count := 0; count < 5; count++ {
			fixedEvents = append(fixedEvents, &cmsRes.Event{
				EventType:       "cookbook",
				Task:            "update",
				Timestamp:       timestamp3,
				EntityName:      "mocked_event",
				RequestorType:   "automatic",
				RequestorName:   "golang",
				ServiceHostname: "localhost",
			})
		}

		return &cmsRes.Events{
			Events:      fixedEvents,
			TotalEvents: 4,
		}, nil
	})

	mockFeedServiceClient := mock_automate_feed.NewMockFeedServiceClient(gomock.NewController(t))
	mockFeedServiceClient.EXPECT().GetFeed(
		context.Background(),
		gomock.Any(),
	).DoAndReturn(func(c context.Context, action *automate_feed.FeedRequest) (*automate_feed.FeedResponse, error) {
		fixedEvents := make([]*automate_feed.FeedEntry, 0)

		for count := 0; count < 5; count++ {
			fixedEvents = append(fixedEvents, &automate_feed.FeedEntry{
				EventType:            "scanJobUpdated",
				Tags:                 []string{"scanjob"},
				Verb:                 "update",
				SourceEventPublished: timestamp2,
				Producer: &automate_feed.Producer{
					Name: "mocked_event",
					ID:   "scanjob",
				},
				Actor: &automate_feed.Actor{
					Name:       "golang",
					ObjectType: "automatic",
				},
				Target: &automate_feed.Target{
					Name: "localhost",
				},
			})
		}

		return &automate_feed.FeedResponse{
			FeedEntries:  fixedEvents,
			TotalEntries: 2,
		}, nil
	})

	eventFeedAggregate := subject.NewEventFeedAggregate(mockCfgMgmtClient, mockFeedServiceClient)
	events, err := eventFeedAggregate.CollectEventFeed(context.Background(), request)

	assert.Nil(t, err)

	assert.Equal(t, 3, len(events.Events), "We should return only 3 events since we ARE collapsing")

	assert.Equal(t, "scanjob", events.Events[1].EventType, "The center event should be the scan job event")

	for _, event := range events.Events {
		assert.Equal(t, int32(5), event.EventCount, "Each collapsed event should have a count of five")
	}
}

// Test the sorting of events. When events have the same time the events will be sorted by their IDs in descending order
func TestEventFeedCollectEventFeedSortedSameDates(t *testing.T) {
	request := &agReq.EventFilter{PageSize: 1000}

	time1 := time.Now()
	timestamp1, err := ptypes.TimestampProto(time1)
	assert.Nil(t, err)

	mockCfgMgmtClient := mock_cfgmgmt.NewMockCfgMgmtClient(gomock.NewController(t))
	mockCfgMgmtClient.EXPECT().GetEventFeed(
		context.Background(),
		gomock.Any(),
	).DoAndReturn(func(c context.Context, action *cmsReq.EventFilter) (*cmsRes.Events, error) {
		fixedEvents := make([]*cmsRes.Event, 0)

		fixedEvents = append(fixedEvents, &cmsRes.Event{
			Id:              "a",
			EventType:       "cookbook",
			Task:            "update",
			Timestamp:       timestamp1,
			EntityName:      "mocked_event",
			RequestorType:   "automatic",
			RequestorName:   "golang",
			ServiceHostname: "localhost",
		})

		fixedEvents = append(fixedEvents, &cmsRes.Event{
			Id:              "c",
			EventType:       "node",
			Task:            "update",
			Timestamp:       timestamp1,
			EntityName:      "mocked_event",
			RequestorType:   "automatic",
			RequestorName:   "golang",
			ServiceHostname: "localhost",
		})

		return &cmsRes.Events{
			Events:      fixedEvents,
			TotalEvents: 2,
		}, nil
	})

	mockFeedServiceClient := mock_automate_feed.NewMockFeedServiceClient((gomock.NewController(t)))
	mockFeedServiceClient.EXPECT().GetFeed(
		context.Background(),
		gomock.Any(),
	).DoAndReturn(func(c context.Context, action *automate_feed.FeedRequest) (*automate_feed.FeedResponse, error) {
		fixedEvents := make([]*automate_feed.FeedEntry, 0)

		fixedEvents = append(fixedEvents, &automate_feed.FeedEntry{
			ID:                   "b",
			EventType:            "scanJobUpdated",
			Tags:                 []string{"scanjob"},
			Verb:                 "update",
			SourceEventPublished: timestamp1,
			Producer: &automate_feed.Producer{
				Name: "mocked_event",
				ID:   "scanjob",
			},
			Actor: &automate_feed.Actor{
				Name:       "golang",
				ObjectType: "automatic",
			},
			Target: &automate_feed.Target{
				Name: "localhost",
			},
		})

		return &automate_feed.FeedResponse{
			FeedEntries:  fixedEvents,
			TotalEntries: 1,
		}, nil
	})

	eventFeedAggregate := subject.NewEventFeedAggregate(mockCfgMgmtClient, mockFeedServiceClient)
	events, err := eventFeedAggregate.CollectEventFeed(context.Background(), request)

	assert.Nil(t, err)

	// in descending order
	assert.Equal(t, "c", events.Events[0].StartId)
	assert.Equal(t, "b", events.Events[1].StartId)
	assert.Equal(t, "a", events.Events[2].StartId)
}

// Testing retrieving the last page in a multiple page collection of events.
// Here the last page is setup to contain the full page size.
// There are 100 events; 50 from compliance 50 in config-mgmt;
// page size is 10.
// compliance and config-mgmt both return 10 events interweaved by date
// Should return 5 of compliance and 5 config-mgmt events
func TestEventFeedCollectEventFeedLastFullPage(t *testing.T) {
	pageSize := int32(10)
	now := time.Now()

	request := &agReq.EventFilter{
		Collapse: false,
		PageSize: pageSize,
		After:    now.UnixNano() / int64(time.Millisecond),
		End:      now.UnixNano() / int64(time.Millisecond),
		Start:    now.AddDate(0, 0, -6).UnixNano() / int64(time.Millisecond),
	}

	mockCfgMgmtClient := mock_cfgmgmt.NewMockCfgMgmtClient(gomock.NewController(t))
	mockCfgMgmtClient.EXPECT().GetEventFeed(
		context.Background(),
		gomock.Any(),
	).DoAndReturn(func(c context.Context, action *cmsReq.EventFilter) (*cmsRes.Events, error) {
		events := make([]*cmsRes.Event, 0)

		for count := 1; count <= 10; count++ {
			// subtract 30 seconds to not have the same time as the compliance events
			current := now.Add((time.Minute * time.Duration(count) * -1) - (time.Second * 30))
			timestamp, err := ptypes.TimestampProto(current)
			assert.Nil(t, err)
			events = append(events, &cmsRes.Event{
				Id:              strconv.Itoa(count),
				EventType:       "cookbook",
				Task:            "update",
				Timestamp:       timestamp,
				EntityName:      "mocked_event",
				RequestorType:   "automatic",
				RequestorName:   "golang",
				ServiceHostname: "localhost",
			})
		}

		return &cmsRes.Events{
			Events:      events,
			TotalEvents: 50,
		}, nil
	})

	mockFeedServiceClient := mock_automate_feed.NewMockFeedServiceClient(gomock.NewController(t))
	mockFeedServiceClient.EXPECT().GetFeed(
		context.Background(),
		gomock.Any(),
	).DoAndReturn(func(c context.Context, action *automate_feed.FeedRequest) (*automate_feed.FeedResponse, error) {
		events := make([]*automate_feed.FeedEntry, 0)

		for count := 1; count <= 10; count++ {
			current := now.Add((time.Minute * time.Duration(count) * -1))
			timestamp, err := ptypes.TimestampProto(current)
			assert.Nil(t, err)
			events = append(events, &automate_feed.FeedEntry{
				ID:                   strconv.Itoa(count),
				EventType:            "scanJobUpdated",
				Tags:                 []string{"scanjob"},
				Verb:                 "update",
				SourceEventPublished: timestamp,
				Producer: &automate_feed.Producer{
					Name: "mocked_event",
					ID:   "scanjob",
				},
				Actor: &automate_feed.Actor{
					Name:       "golang",
					ObjectType: "automatic",
				},
				Target: &automate_feed.Target{
					Name: "localhost",
				},
			})
		}

		return &automate_feed.FeedResponse{
			FeedEntries:  events,
			TotalEntries: 50,
		}, nil
	})

	eventFeedAggregate := subject.NewEventFeedAggregate(mockCfgMgmtClient, mockFeedServiceClient)
	events, err := eventFeedAggregate.CollectEventFeed(context.Background(), request)

	assert.Nil(t, err)

	assert.Equal(t, int(pageSize), len(events.Events), "Number of events should equal the pageSize")

	// Should be the oldest events in descending order
	assert.Equal(t, "6", events.Events[0].StartId)
	assert.Equal(t, "scanjob", events.Events[0].EventType)

	assert.Equal(t, "6", events.Events[1].StartId)
	assert.Equal(t, "cookbook", events.Events[1].EventType)

	assert.Equal(t, "7", events.Events[2].StartId)
	assert.Equal(t, "scanjob", events.Events[2].EventType)

	assert.Equal(t, "7", events.Events[3].StartId)
	assert.Equal(t, "cookbook", events.Events[3].EventType)

	assert.Equal(t, "8", events.Events[4].StartId)
	assert.Equal(t, "scanjob", events.Events[4].EventType)

	assert.Equal(t, "8", events.Events[5].StartId)
	assert.Equal(t, "cookbook", events.Events[5].EventType)

	assert.Equal(t, "9", events.Events[6].StartId)
	assert.Equal(t, "scanjob", events.Events[6].EventType)

	assert.Equal(t, "9", events.Events[7].StartId)
	assert.Equal(t, "cookbook", events.Events[7].EventType)

	assert.Equal(t, "10", events.Events[8].StartId)
	assert.Equal(t, "scanjob", events.Events[8].EventType)

	assert.Equal(t, "10", events.Events[9].StartId)
	assert.Equal(t, "cookbook", events.Events[9].EventType)
}

// Testing retrieving the last page in a multiple page collection of events.
// Here the last page is setup to contain the full page size.
// There are 100 events; 50 from compliance 45 in config-mgmt;
// page size is 10.
// compliance and config-mgmt both return 10 events interweaved by date
// 5 of compliance and 5 config-mgmt events
func TestEventFeedCollectEventFeedLastHalfFullPage(t *testing.T) {
	pageSize := int32(10)

	now := time.Now()

	request := &agReq.EventFilter{
		Collapse: false,
		PageSize: pageSize,
		After:    now.UnixNano() / int64(time.Millisecond),
		End:      now.UnixNano() / int64(time.Millisecond),
		Start:    now.AddDate(0, 0, -6).UnixNano() / int64(time.Millisecond),
	}

	configMgmtTotalEvents := int64(50)
	complianceTotalEvents := int64(45)
	combinedTotalEvents := configMgmtTotalEvents + complianceTotalEvents

	mockCfgMgmtClient := mock_cfgmgmt.NewMockCfgMgmtClient(gomock.NewController(t))
	mockCfgMgmtClient.EXPECT().GetEventFeed(
		context.Background(),
		gomock.Any(),
	).DoAndReturn(func(c context.Context, action *cmsReq.EventFilter) (*cmsRes.Events, error) {
		events := make([]*cmsRes.Event, 0)

		for count := 1; count <= 10; count++ {
			// subtract 30 seconds to not have the same time as the compliance events
			current := now.Add((time.Minute * time.Duration(count) * -1) - (time.Second * 30))
			timestamp, err := ptypes.TimestampProto(current)
			assert.Nil(t, err)
			events = append(events, &cmsRes.Event{
				Id:              strconv.Itoa(count),
				EventType:       "cookbook",
				Task:            "update",
				Timestamp:       timestamp,
				EntityName:      "mocked_event",
				RequestorType:   "automatic",
				RequestorName:   "golang",
				ServiceHostname: "localhost",
			})
		}

		return &cmsRes.Events{
			Events:      events,
			TotalEvents: configMgmtTotalEvents,
		}, nil
	})

	mockFeedServiceClient := mock_automate_feed.NewMockFeedServiceClient(gomock.NewController(t))
	mockFeedServiceClient.EXPECT().GetFeed(
		context.Background(),
		gomock.Any(),
	).DoAndReturn(func(c context.Context, action *automate_feed.FeedRequest) (*automate_feed.FeedResponse, error) {
		events := make([]*automate_feed.FeedEntry, 0)

		for count := 1; count <= 10; count++ {
			current := now.Add((time.Minute * time.Duration(count) * -1))
			timestamp, err := ptypes.TimestampProto(current)
			assert.Nil(t, err)
			events = append(events, &automate_feed.FeedEntry{
				ID:                   strconv.Itoa(count),
				EventType:            "scanJobUpdated",
				Tags:                 []string{"scanjob"},
				Verb:                 "update",
				SourceEventPublished: timestamp,
				Producer: &automate_feed.Producer{
					Name: "mocked_event",
					ID:   "scanjob",
				},
				Actor: &automate_feed.Actor{
					Name:       "golang",
					ObjectType: "automatic",
				},
				Target: &automate_feed.Target{
					Name: "localhost",
				},
			})
		}

		return &automate_feed.FeedResponse{
			FeedEntries:  events,
			TotalEntries: complianceTotalEvents,
		}, nil
	})

	eventFeedAggregate := subject.NewEventFeedAggregate(mockCfgMgmtClient, mockFeedServiceClient)
	events, err := eventFeedAggregate.CollectEventFeed(context.Background(), request)

	assert.Nil(t, err)

	assert.Equal(t, int(int32(combinedTotalEvents)%pageSize), len(events.Events),
		"Number of events should equal the reminder of the page size")

	// Should be the oldest events in descending order
	assert.Equal(t, "8", events.Events[0].StartId)
	assert.Equal(t, "cookbook", events.Events[0].EventType)

	assert.Equal(t, "9", events.Events[1].StartId)
	assert.Equal(t, "scanjob", events.Events[1].EventType)

	assert.Equal(t, "9", events.Events[2].StartId)
	assert.Equal(t, "cookbook", events.Events[2].EventType)

	assert.Equal(t, "10", events.Events[3].StartId)
	assert.Equal(t, "scanjob", events.Events[3].EventType)

	assert.Equal(t, "10", events.Events[4].StartId)
	assert.Equal(t, "cookbook", events.Events[4].EventType)
}

// Test when a next page request is made on a page break with a mixed collection of
// events with the same timestamp from both compliance and config-mgmt.
func TestEventFeedCollectEventFeedSameDatesNextFirstSecondPage(t *testing.T) {
	pageSize := int32(10)

	now := time.Now()

	beforePageBreakDate := now.AddDate(0, 0, -1)
	cursorId := "d"

	request := &agReq.EventFilter{
		Collapse: false,
		PageSize: pageSize,
		Before:   beforePageBreakDate.UnixNano() / int64(time.Millisecond),
		Cursor:   cursorId,
		End:      now.UnixNano() / int64(time.Millisecond),
		Start:    now.AddDate(0, 0, -6).UnixNano() / int64(time.Millisecond),
	}

	mockCfgMgmtClient := mock_cfgmgmt.NewMockCfgMgmtClient(gomock.NewController(t))
	mockCfgMgmtClient.EXPECT().GetEventFeed(
		context.Background(),
		gomock.Any(),
	).DoAndReturn(func(c context.Context, action *cmsReq.EventFilter) (*cmsRes.Events, error) {
		events := make([]*cmsRes.Event, 0)

		timestamp, err := ptypes.TimestampProto(beforePageBreakDate)
		assert.Nil(t, err)
		for count := 0; count < 10; count++ {
			events = append(events, &cmsRes.Event{
				Id:              "b" + strconv.Itoa(count),
				EventType:       "cookbook",
				Task:            "update",
				Timestamp:       timestamp,
				EntityName:      "mocked_event",
				RequestorType:   "automatic",
				RequestorName:   "golang",
				ServiceHostname: "localhost",
			})
		}

		return &cmsRes.Events{
			Events:      events,
			TotalEvents: 50,
		}, nil
	})

	mockFeedServiceClient := mock_automate_feed.NewMockFeedServiceClient(gomock.NewController(t))
	mockFeedServiceClient.EXPECT().GetFeed(
		context.Background(),
		gomock.Any(),
	).DoAndReturn(func(c context.Context, action *automate_feed.FeedRequest) (*automate_feed.FeedResponse, error) {
		events := make([]*automate_feed.FeedEntry, 0)

		timestamp, err := ptypes.TimestampProto(beforePageBreakDate)
		assert.Nil(t, err)
		for count := 0; count < 10; count++ {
			events = append(events, &automate_feed.FeedEntry{
				ID:                   "a" + strconv.Itoa(count),
				EventType:            "scanJobUpdated",
				Tags:                 []string{"scanjob"},
				Verb:                 "update",
				SourceEventPublished: timestamp,
				Producer: &automate_feed.Producer{
					Name: "mocked_event",
					ID:   "scanjob",
				},
				Actor: &automate_feed.Actor{
					Name:       "golang",
					ObjectType: "automatic",
				},
				Target: &automate_feed.Target{
					Name: "localhost",
				},
			})
		}

		return &automate_feed.FeedResponse{
			FeedEntries:  events,
			TotalEntries: 50,
		}, nil
	})

	eventFeedAggregate := subject.NewEventFeedAggregate(mockCfgMgmtClient, mockFeedServiceClient)
	events, err := eventFeedAggregate.CollectEventFeed(context.Background(), request)

	assert.Nil(t, err)

	// Should return only config-mgmt's events because when sorted by ID they are
	// greater than all the compliance events.
	assert.Equal(t, "b9", events.Events[0].StartId)
	assert.Equal(t, "cookbook", events.Events[0].EventType)

	assert.Equal(t, "b8", events.Events[1].StartId)
	assert.Equal(t, "cookbook", events.Events[1].EventType)

	assert.Equal(t, "b7", events.Events[2].StartId)
	assert.Equal(t, "cookbook", events.Events[2].EventType)

	assert.Equal(t, "b6", events.Events[3].StartId)
	assert.Equal(t, "cookbook", events.Events[3].EventType)

	assert.Equal(t, "b5", events.Events[4].StartId)
	assert.Equal(t, "cookbook", events.Events[4].EventType)
}

// Test when a previous page request is made on a page break with a mixed collection of
// events with the same timestamp from both compliance and config-mgmt.
// The all the events should be sorted like:
// f9,f8,f7,f6,f5,f4,f3,f2,f1,f0,e9,e8,e7,e6,e5,e4,e3,e2,e1,e0,d in descending order
// We want the events after 'd' that would be: e9,e8,e7,e6,e5,e4,e3,e2,e1,e0
func TestEventFeedCollectEventFeedSameDatesPreviousThirdToSecondPage(t *testing.T) {
	pageSize := int32(10)

	now := time.Now()

	afterPageBreakDate := now.AddDate(0, 0, -3)
	cursorId := "d"

	request := &agReq.EventFilter{
		Collapse: false,
		PageSize: pageSize,
		After:    afterPageBreakDate.UnixNano() / int64(time.Millisecond),
		Cursor:   cursorId,
		End:      now.UnixNano() / int64(time.Millisecond),
		Start:    now.AddDate(0, 0, -6).UnixNano() / int64(time.Millisecond),
	}

	mockCfgMgmtClient := mock_cfgmgmt.NewMockCfgMgmtClient(gomock.NewController(t))
	mockCfgMgmtClient.EXPECT().GetEventFeed(
		context.Background(),
		gomock.Any(),
	).DoAndReturn(func(c context.Context, action *cmsReq.EventFilter) (*cmsRes.Events, error) {
		events := make([]*cmsRes.Event, 0)

		timestamp, err := ptypes.TimestampProto(afterPageBreakDate)
		assert.Nil(t, err)
		for count := 0; count < 10; count++ {
			events = append(events, &cmsRes.Event{
				Id:              "e" + strconv.Itoa(count),
				EventType:       "cookbook",
				Task:            "update",
				Timestamp:       timestamp,
				EntityName:      "mocked_event",
				RequestorType:   "automatic",
				RequestorName:   "golang",
				ServiceHostname: "localhost",
			})
		}

		return &cmsRes.Events{
			Events:      events,
			TotalEvents: 50,
		}, nil
	})

	mockFeedServiceClient := mock_automate_feed.NewMockFeedServiceClient(gomock.NewController(t))
	mockFeedServiceClient.EXPECT().GetFeed(
		context.Background(),
		gomock.Any(),
	).DoAndReturn(func(c context.Context, action *automate_feed.FeedRequest) (*automate_feed.FeedResponse, error) {
		events := make([]*automate_feed.FeedEntry, 0)

		timestamp, err := ptypes.TimestampProto(afterPageBreakDate)
		assert.Nil(t, err)
		for count := 0; count < 10; count++ {
			events = append(events, &automate_feed.FeedEntry{
				ID:                   "f" + strconv.Itoa(count),
				EventType:            "scanJobUpdated",
				Tags:                 []string{"scanjob"},
				Verb:                 "update",
				SourceEventPublished: timestamp,
				Producer: &automate_feed.Producer{
					Name: "mocked_event",
					ID:   "scanjob",
				},
				Actor: &automate_feed.Actor{
					Name:       "golang",
					ObjectType: "automatic",
				},
				Target: &automate_feed.Target{
					Name: "localhost",
				},
			})
		}

		return &automate_feed.FeedResponse{
			FeedEntries:  events,
			TotalEntries: 50,
		}, nil
	})

	eventFeedAggregate := subject.NewEventFeedAggregate(mockCfgMgmtClient, mockFeedServiceClient)
	events, err := eventFeedAggregate.CollectEventFeed(context.Background(), request)

	assert.Nil(t, err)

	// Should return only config-mgmt's events because when sorted by ID they are
	// less than all the compliance events.
	assert.Equal(t, "e9", events.Events[0].StartId)
	assert.Equal(t, "cookbook", events.Events[0].EventType)

	assert.Equal(t, "e8", events.Events[1].StartId)
	assert.Equal(t, "cookbook", events.Events[1].EventType)

	assert.Equal(t, "e7", events.Events[2].StartId)
	assert.Equal(t, "cookbook", events.Events[2].EventType)

	assert.Equal(t, "e6", events.Events[3].StartId)
	assert.Equal(t, "cookbook", events.Events[3].EventType)

	assert.Equal(t, "e5", events.Events[4].StartId)
	assert.Equal(t, "cookbook", events.Events[4].EventType)

	assert.Equal(t, "e4", events.Events[5].StartId)
	assert.Equal(t, "cookbook", events.Events[5].EventType)

	assert.Equal(t, "e3", events.Events[6].StartId)
	assert.Equal(t, "cookbook", events.Events[6].EventType)

	assert.Equal(t, "e2", events.Events[7].StartId)
	assert.Equal(t, "cookbook", events.Events[7].EventType)

	assert.Equal(t, "e1", events.Events[8].StartId)
	assert.Equal(t, "cookbook", events.Events[8].EventType)

	assert.Equal(t, "e0", events.Events[9].StartId)
	assert.Equal(t, "cookbook", events.Events[9].EventType)
}

func TestEventFeedCollectEventFeedReturnErrorWithWrongParameters(t *testing.T) {
	var (
		date = time.Now()
	)
	cases := []struct {
		description string
		request     agReq.EventFilter
	}{
		{
			description: "The Start date is after the End date",
			request: agReq.EventFilter{
				End:      date.AddDate(0, 0, -6).Unix() * 1000,
				Start:    date.Unix() * 1000,
				PageSize: 10,
			},
		},
		{
			description: "Before and After parameters should not both be set",
			request: agReq.EventFilter{
				End:      date.Unix() * 1000,
				Start:    date.AddDate(0, 0, -6).Unix() * 1000,
				PageSize: 10,
				Before:   date.AddDate(0, 0, -3).Unix() * 1000,
				After:    date.AddDate(0, 0, -1).Unix() * 1000,
			},
		},
		{
			description: "If the Before param is set the Cursor param needs to be set also",
			request: agReq.EventFilter{
				End:      date.Unix() * 1000,
				Start:    date.AddDate(0, 0, -6).Unix() * 1000,
				PageSize: 10,
				Before:   date.AddDate(0, 0, -3).Unix() * 1000,
			},
		},
		{
			description: "When the After is set without the Cursor, After must be equal to End",
			request: agReq.EventFilter{
				End:      date.Unix() * 1000,
				Start:    date.AddDate(0, 0, -6).Unix() * 1000,
				PageSize: 10,
				After:    date.AddDate(0, 0, -3).Unix() * 1000,
			},
		},
		{
			description: "The Page size is set to zero",
			request: agReq.EventFilter{
				End:      date.Unix() * 1000,
				Start:    date.AddDate(0, 0, -6).Unix() * 1000,
				PageSize: 0,
			},
		},
	}

	for _, test := range cases {
		t.Run(fmt.Sprintf("with parameters filters=%v it %s should return an error",
			test.request, test.description), func(t *testing.T) {
			ctrl := gomock.NewController(t)

			mockCfgMgmtClient := mock_cfgmgmt.NewMockCfgMgmtClient(ctrl)
			mockCfgMgmtClient.EXPECT().GetEventFeed(
				context.Background(),
				gomock.Any(),
			).DoAndReturn(func(c context.Context, action *cmsReq.EventFilter) (*cmsRes.Events, error) {
				return &cmsRes.Events{}, nil
			})

			mockFeedServiceClient := mock_automate_feed.NewMockFeedServiceClient(ctrl)
			mockFeedServiceClient.EXPECT().GetFeed(
				context.Background(),
				gomock.Any(),
			).DoAndReturn(func(c context.Context, action *automate_feed.FeedRequest) (*automate_feed.FeedResponse, error) {
				return &automate_feed.FeedResponse{}, nil
			})

			eventFeedAggregate := subject.NewEventFeedAggregate(mockCfgMgmtClient, mockFeedServiceClient)

			_, err := eventFeedAggregate.CollectEventFeed(
				context.Background(),
				&test.request,
			)

			if assert.NotNil(t, err) {
				assert.Contains(t, err.Error(), "rpc error:")
				errStatus, ok := status.FromError(err)
				if assert.True(t, ok) {
					assert.Equal(t, codes.InvalidArgument, errStatus.Code())
				}
			}
		})
	}
}

// cmsFixedEvents returns a fixed set of events. (101)
func cmsFixedEvents(t *testing.T) *cmsRes.Events {
	time1, err := time.Parse(time.RFC3339, "2018-01-30T21:21:21Z")
	assert.Nil(t, err)
	timestamp1, err := ptypes.TimestampProto(time1)
	assert.Nil(t, err)
	time2, err := time.Parse(time.RFC3339, "2018-01-29T21:20:41Z")
	assert.Nil(t, err)
	timestamp2, err := ptypes.TimestampProto(time2)
	assert.Nil(t, err)
	time3, err := time.Parse(time.RFC3339, "2018-01-30T21:20:41Z")
	assert.Nil(t, err)
	timestamp3, err := ptypes.TimestampProto(time3)
	assert.Nil(t, err)
	var (
		fixedEvents = []*cmsRes.Event{
			&cmsRes.Event{
				EventType:       "node",
				Task:            "update",
				Timestamp:       timestamp1,
				EntityName:      "mocked_event",
				RequestorType:   "automatic",
				RequestorName:   "golang",
				ServiceHostname: "localhost",
			},
		}
		// This is the event type we will add multiple times to group it or not
		cookbookUpdateEvent = &cmsRes.Event{
			EventType:       "cookbook",
			Task:            "update",
			Timestamp:       timestamp2,
			EntityName:      "mocked_event",
			RequestorType:   "automatic",
			RequestorName:   "golang",
			ServiceHostname: "localhost",
		}

		laterCookbookUpdateEvent = &cmsRes.Event{
			EventType:       "cookbook",
			Task:            "update",
			Timestamp:       timestamp3,
			EntityName:      "mocked_event",
			RequestorType:   "automatic",
			RequestorName:   "golang",
			ServiceHostname: "localhost",
		}
	)

	// Adding 9 cookbook events (updated by golang, since this is an automated test)
	for i := 0; i < 99; i++ {
		fixedEvents = append(fixedEvents, cookbookUpdateEvent)
	}

	//Add a final cookbook with a later timestamp to test endtime.
	fixedEvents = append(fixedEvents, laterCookbookUpdateEvent)

	// We are always returning 100 cookbook events plus 1 node edit
	// => 101 Events

	// sort fixedEvents
	sort.Slice(fixedEvents[:], func(i, j int) bool {
		idate, err := ptypes.Timestamp(fixedEvents[i].Timestamp)
		assert.Nil(t, err)
		jdate, err := ptypes.Timestamp(fixedEvents[j].Timestamp)
		assert.Nil(t, err)
		return idate.After(jdate)
	})

	return &cmsRes.Events{Events: fixedEvents}
}

// complianceEventFixedEvents returns a fixed set of events. (101)
func complianceEventFixedEvents(t *testing.T) *automate_feed.FeedResponse {
	time1, err := time.Parse(time.RFC3339, "2018-01-20T21:21:21Z")
	assert.Nil(t, err)
	timestamp1, err := ptypes.TimestampProto(time1)
	assert.Nil(t, err)
	time2, err := time.Parse(time.RFC3339, "2018-01-19T21:20:41Z")
	assert.Nil(t, err)
	timestamp2, err := ptypes.TimestampProto(time2)
	assert.Nil(t, err)
	time3, err := time.Parse(time.RFC3339, "2018-01-20T21:20:41Z")
	assert.Nil(t, err)
	timestamp3, err := ptypes.TimestampProto(time3)
	assert.Nil(t, err)

	var (
		fixedEvents = []*automate_feed.FeedEntry{
			&automate_feed.FeedEntry{
				EventType:            "scanJobUpdated",
				Tags:                 []string{"scanjob"},
				Verb:                 "update",
				SourceEventPublished: timestamp1,
				Producer: &automate_feed.Producer{
					Name: "mocked_event",
					ID:   "scanjob",
				},
				Actor: &automate_feed.Actor{
					Name:       "golang",
					ObjectType: "automatic",
				},
				Target: &automate_feed.Target{
					Name: "localhost",
				},
			},
		}
		// This is the event type we will add multiple times to group it or not
		profileUpdateEvent = &automate_feed.FeedEntry{
			EventType:            "profileUpdated",
			Tags:                 []string{"profile"},
			Verb:                 "update",
			SourceEventPublished: timestamp2,
			Producer: &automate_feed.Producer{
				Name: "mocked_event",
				ID:   "profile",
			},
			Actor: &automate_feed.Actor{
				Name:       "golang",
				ObjectType: "automatic",
			},
			Target: &automate_feed.Target{
				Name: "localhost",
			},
		}

		laterProfileUpdateEvent = &automate_feed.FeedEntry{
			EventType:            "profileUpdated",
			Tags:                 []string{"profile"},
			Verb:                 "update",
			SourceEventPublished: timestamp3,
			Producer: &automate_feed.Producer{
				Name: "mocked_event",
				ID:   "profile",
			},
			Actor: &automate_feed.Actor{
				Name:       "golang",
				ObjectType: "automatic",
			},
			Target: &automate_feed.Target{
				Name: "localhost",
			},
		}
	)

	// Adding 9 profile events (updated by golang, since this is an automated test)
	for i := 0; i < 99; i++ {
		fixedEvents = append(fixedEvents, profileUpdateEvent)
	}

	//Add a final profile with a later timestamp to test endtime.
	fixedEvents = append(fixedEvents, laterProfileUpdateEvent)

	// We are always returning 100 profile events plus 1 node edit
	// => 101 Events

	// sort fixedEvents
	sort.Slice(fixedEvents[:], func(i, j int) bool {
		idate, err := ptypes.Timestamp(fixedEvents[i].SourceEventPublished)
		assert.Nil(t, err)
		jdate, err := ptypes.Timestamp(fixedEvents[j].SourceEventPublished)
		assert.Nil(t, err)
		return idate.After(jdate)
	})

	return &automate_feed.FeedResponse{FeedEntries: fixedEvents}
}
