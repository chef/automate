package eventfeed_test

import (
	"context"
	"fmt"
	"sort"
	"testing"
	"time"

	agReq "github.com/chef/automate/api/external/event_feed/request"
	event_feed_api "github.com/chef/automate/api/interservice/event_feed"
	subject "github.com/chef/automate/components/automate-gateway/eventfeed"
	"github.com/golang/mock/gomock"
	"github.com/golang/protobuf/ptypes"
	"github.com/stretchr/testify/assert"
	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/status"
)

func TestEventFeedEmpty(t *testing.T) {
	ctrl := gomock.NewController(t)

	mockFeedServiceClient := event_feed_api.NewMockEventFeedServiceClient(ctrl)
	mockFeedServiceClient.EXPECT().GetFeed(
		context.Background(),
		gomock.Any(),
	).DoAndReturn(func(c context.Context, action *event_feed_api.FeedRequest) (*event_feed_api.FeedResponse, error) {
		return &event_feed_api.FeedResponse{}, nil
	})

	eventFeedAggregate := subject.NewEventFeedAggregate(mockFeedServiceClient)

	eventCounts, err := eventFeedAggregate.CollectEventFeed(
		context.Background(),
		&agReq.GetEventFeedRequest{
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

	mockFeedServiceClient := event_feed_api.NewMockEventFeedServiceClient(ctrl)
	mockFeedServiceClient.EXPECT().GetFeed(
		context.Background(),
		gomock.Any(),
	).DoAndReturn(func(c context.Context, action *event_feed_api.FeedRequest) (*event_feed_api.FeedResponse, error) {
		return fixedEvents(t), nil
	})

	eventFeedAggregate := subject.NewEventFeedAggregate(mockFeedServiceClient)
	events, err := eventFeedAggregate.CollectEventFeed(context.Background(),
		&agReq.GetEventFeedRequest{
			Collapse: false,
			PageSize: 1000,
		},
	)

	assert.Nil(t, err)
	// We should return all 202 events, we are NOT collapsing
	assert.Equal(t, 101, len(events.Events))
}

// TestEventFeedCollectEventFeedCollapseTrue Sending collapse=true
func TestEventFeedCollectEventFeedCollapseNonoverlapping(t *testing.T) {
	// Enable event grouping
	request := &agReq.GetEventFeedRequest{Collapse: true, PageSize: 1000}

	mockFeedServiceClient := event_feed_api.NewMockEventFeedServiceClient(gomock.NewController(t))
	mockFeedServiceClient.EXPECT().GetFeed(
		context.Background(),
		gomock.Any(),
	).DoAndReturn(func(c context.Context, action *event_feed_api.FeedRequest) (*event_feed_api.FeedResponse, error) {
		return fixedEvents(t), nil
	})

	eventFeedAggregate := subject.NewEventFeedAggregate(mockFeedServiceClient)
	events, err := eventFeedAggregate.CollectEventFeed(context.Background(), request)

	assert.Nil(t, err)
	// We should return only 2 events since we ARE collapsing
	assert.Equal(t, 2, len(events.Events))
	assert.Equal(t, "2018-01-20T21:20:41Z", ptypes.TimestampString(events.Events[1].EndTime))
}

func TestEventFeedCollectEventFeedReturnErrorWithWrongParameters(t *testing.T) {
	var (
		date = time.Now()
	)
	cases := []struct {
		description string
		request     agReq.GetEventFeedRequest
	}{
		{
			description: "The Start date is after the End date",
			request: agReq.GetEventFeedRequest{
				End:      date.AddDate(0, 0, -6).Unix() * 1000,
				Start:    date.Unix() * 1000,
				PageSize: 10,
			},
		},
		{
			description: "Before and After parameters should not both be set",
			request: agReq.GetEventFeedRequest{
				End:      date.Unix() * 1000,
				Start:    date.AddDate(0, 0, -6).Unix() * 1000,
				PageSize: 10,
				Before:   date.AddDate(0, 0, -3).Unix() * 1000,
				After:    date.AddDate(0, 0, -1).Unix() * 1000,
			},
		},
		{
			description: "If the Before param is set the Cursor param needs to be set also",
			request: agReq.GetEventFeedRequest{
				End:      date.Unix() * 1000,
				Start:    date.AddDate(0, 0, -6).Unix() * 1000,
				PageSize: 10,
				Before:   date.AddDate(0, 0, -3).Unix() * 1000,
			},
		},
		{
			description: "When the After is set without the Cursor, After must be equal to End",
			request: agReq.GetEventFeedRequest{
				End:      date.Unix() * 1000,
				Start:    date.AddDate(0, 0, -6).Unix() * 1000,
				PageSize: 10,
				After:    date.AddDate(0, 0, -3).Unix() * 1000,
			},
		},
		{
			description: "The Page size is set to zero",
			request: agReq.GetEventFeedRequest{
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

			mockFeedServiceClient := event_feed_api.NewMockEventFeedServiceClient(ctrl)
			mockFeedServiceClient.EXPECT().GetFeed(
				context.Background(),
				gomock.Any(),
			).DoAndReturn(func(c context.Context, action *event_feed_api.FeedRequest) (*event_feed_api.FeedResponse, error) {
				return &event_feed_api.FeedResponse{}, nil
			})

			eventFeedAggregate := subject.NewEventFeedAggregate(mockFeedServiceClient)

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

// fixedEvents returns a fixed set of events. (101)
func fixedEvents(t *testing.T) *event_feed_api.FeedResponse {
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
		fixedEvents = []*event_feed_api.FeedEntry{
			{
				EventType:            "scanJobUpdated",
				Tags:                 []string{"scanjob"},
				Verb:                 "update",
				SourceEventPublished: timestamp1,
				Producer: &event_feed_api.Producer{
					Name: "mocked_event",
					Id:   "scanjob",
				},
				Actor: &event_feed_api.Actor{
					Name:       "golang",
					ObjectType: "automatic",
				},
				Target: &event_feed_api.Target{
					Name: "localhost",
				},
			},
		}
		// This is the event type we will add multiple times to group it or not
		profileUpdateEvent = &event_feed_api.FeedEntry{
			EventType:            "profileUpdated",
			Tags:                 []string{"profile"},
			Verb:                 "update",
			SourceEventPublished: timestamp2,
			Producer: &event_feed_api.Producer{
				Name: "mocked_event",
				Id:   "profile",
			},
			Actor: &event_feed_api.Actor{
				Name:       "golang",
				ObjectType: "automatic",
			},
			Target: &event_feed_api.Target{
				Name: "localhost",
			},
		}

		laterProfileUpdateEvent = &event_feed_api.FeedEntry{
			EventType:            "profileUpdated",
			Tags:                 []string{"profile"},
			Verb:                 "update",
			SourceEventPublished: timestamp3,
			Producer: &event_feed_api.Producer{
				Name: "mocked_event",
				Id:   "profile",
			},
			Actor: &event_feed_api.Actor{
				Name:       "golang",
				ObjectType: "automatic",
			},
			Target: &event_feed_api.Target{
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

	return &event_feed_api.FeedResponse{FeedEntries: fixedEvents}
}
