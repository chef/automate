package integration_test

import (
	"context"
	"fmt"
	"strconv"
	"testing"
	"time"

	"github.com/gofrs/uuid"
	"github.com/golang/protobuf/ptypes"
	log "github.com/sirupsen/logrus"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
	"google.golang.org/grpc"

	api "github.com/chef/automate/api/interservice/event"
	event_feed_api "github.com/chef/automate/api/interservice/event_feed"
	"github.com/chef/automate/components/event-feed-service/pkg/persistence"
	"github.com/chef/automate/components/event-service/server"
)

var (
	feedClient  event_feed_api.EventFeedServiceClient
	eventClient api.EventServiceClient
)

func TestPublish(t *testing.T) {

	// EventMsg
	pub := ptypes.TimestampNow()

	// util.FeedEntry
	publishedAt, err := ptypes.Timestamp(pub)
	require.NoErrorf(t, err, "Unable to translate source event publish timestamp %q to time.Time", pub.String())

	// util.FeedEntry to FeedEntry proto
	pubTs, err := ptypes.TimestampProto(publishedAt.UTC())
	require.NoErrorf(t, err, "Unable to translate time.Time %q to timestamp proto", publishedAt.UTC().String())

	event := api.EventMsg{
		EventID: uuid.Must(uuid.NewV4()).String(),
		Type:    &api.EventType{Name: server.ScanJobCreated},
		Producer: &api.Producer{
			ID:           "urn:chef:compliance:scan-component",
			ProducerName: "Scanner",
			ProducerType: "system component",
		},
		Tags:      []string{"scanjobs", "create"},
		Published: pub,
		Actor: &api.Actor{
			ID:          "urn:mycompany:user:fred",
			ObjectType:  "User",
			DisplayName: "Fred",
		},
		Verb: "create",
		Object: &api.Object{
			ID:          uuid.Must(uuid.NewV4()).String(),
			ObjectType:  "scanjobs", // entity types are scanjobs, profile
			DisplayName: "Scan Job",
		},
		Target: &api.Target{
			ID:          "urn:mycompany:environment:production",
			ObjectType:  "Environment",
			DisplayName: "Production",
		},
	}

	fe := event_feed_api.FeedEntry{
		ID:                   "unknown; assigned at feed entry creation time",
		EventType:            event.Type.Name,
		FeedType:             "event",
		Tags:                 event.Tags,
		SourceEventPublished: pubTs,
		Producer: &event_feed_api.Producer{
			ID:         event.Producer.ID,
			Name:       event.Producer.ProducerName,
			ObjectType: event.Producer.ProducerType,
			PTags:      event.Producer.Tags,
		},
		Actor: &event_feed_api.Actor{
			ID:         event.Actor.ID,
			Name:       event.Actor.DisplayName,
			ObjectType: event.Actor.ObjectType,
		},
		Verb: event.Verb,
		Object: &event_feed_api.Object{
			ID:         event.Object.ID,
			Name:       event.Object.DisplayName,
			ObjectType: event.Object.ObjectType,
		},
		Target: &event_feed_api.Target{
			ID:         event.Target.ID,
			ObjectType: event.Object.ObjectType,
			Name:       event.Target.DisplayName,
		},
	}

	entries := []*event_feed_api.FeedEntry{&fe}
	fr := event_feed_api.FeedResponse{TotalEntries: 1, FeedEntries: entries}

	req := api.PublishRequest{Msg: &event}

	tests := []struct {
		name string
		in   *api.PublishRequest
		out  bool
		res  *event_feed_api.FeedResponse
		e    *event_feed_api.FeedEntry
		num  int
	}{
		{
			name: "publish 1 scan job created event, get 1 scan job created feed entry",
			in:   &req,
			out:  true,
			res:  &fr,
			e:    &fe,
			num:  1,
		},
	}

	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			ctx := context.Background()

			resp, err := getEventClient().Publish(ctx, test.in)
			require.NoErrorf(t, err, "publishing %v", test.in)

			time.Sleep(6 * time.Second)
			suite.refreshIndices(persistence.IndexNameFeeds)

			feedResp, err := getFeedClient().GetFeed(ctx, &event_feed_api.FeedRequest{Size: 10})
			require.NoError(t, err)

			assert.True(t, resp.Success, "Expected publish to return with success == true")
			require.Equal(t, test.res.TotalEntries, feedResp.TotalEntries)

			if assert.NotNil(t, feedResp.FeedEntries) && assert.Equal(t, test.num, len(feedResp.FeedEntries)) {
				got := feedResp.FeedEntries[0]
				assert.Equal(t, test.e.EventType, got.EventType)
				assert.Equal(t, test.e.FeedType, got.FeedType)
				assert.Equal(t, test.e.Tags, got.Tags)
				assert.Equal(t, test.e.SourceEventPublished, got.SourceEventPublished)
				assert.Equal(t, test.e.Producer.ID, got.Producer.ID)
				assert.Equal(t, test.e.Actor.ID, got.Actor.ID)
				assert.Equal(t, test.e.Verb, got.Verb)
				assert.Equal(t, test.e.Object.ID, got.Object.ID)
				assert.Equal(t, test.e.Target.ID, got.Target.ID)
			}
		})
		suite.deleteAllDocuments()
		suite.refreshIndices(persistence.IndexNameFeeds)
	}
}

func TestPublishNodeTerminated(t *testing.T) {
	event := api.EventMsg{
		EventID: uuid.Must(uuid.NewV4()).String(),
		Type:    &api.EventType{Name: server.NodeTerminated},
		Producer: &api.Producer{
			ID:           "urn:chef:compliance:mgrpolling",
			ProducerName: "Node Manager Polling",
			ProducerType: "system component",
		},
		Tags:      []string{"nodemgr", "terminate"},
		Published: ptypes.TimestampNow(),
		Actor: &api.Actor{
			ID:          "",
			ObjectType:  "nodemanager",
			DisplayName: "nodemanager",
		},
		Verb: "terminate",
		Object: &api.Object{
			ID:          "instanceID",
			ObjectType:  "instance ID",
			DisplayName: "instanceID",
		},
		Target: &api.Target{
			ID:          "",
			ObjectType:  "Not Applicable",
			DisplayName: "Not Applicable",
		},
	}
	req := api.PublishRequest{Msg: &event}

	tests := []struct {
		name string
		in   *api.PublishRequest
		out  bool
	}{
		{
			name: "happy path",
			in:   &req,
			out:  true,
		},
	}

	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			resp, err := getEventClient().Publish(context.Background(), test.in)
			require.NoError(t, err, "Error publishing event via event client")
			assert.Equal(t, test.out, resp.Success)
		})
	}
}

func TestPublishLoad(t *testing.T) {
	numEvents := 1500
	events := suite.createEvents(numEvents)
	tests := []struct {
		name               string
		in                 []*api.EventMsg
		out                bool
		expectedEntryCount int64
	}{
		{
			name:               "publish " + strconv.Itoa(numEvents) + " scan job created events, get feed entry count of " + strconv.Itoa(numEvents),
			in:                 events,
			out:                true,
			expectedEntryCount: int64(numEvents),
		},
	}

	for _, test := range tests {
		t.Logf("case: %s", test.name)
		ctx := context.Background()

		for _, e := range test.in {
			pr := api.PublishRequest{Msg: e}
			_, err := getEventClient().Publish(ctx, &pr)
			if err != nil {
				log.WithFields(log.Fields{
					"err": err,
				}).Fatal(fmt.Sprintf("error publishing event"))
			}
		}
		time.Sleep(10 * time.Second)
		suite.refreshIndices(persistence.IndexNameFeeds)

		fsr := &event_feed_api.FeedSummaryRequest{CountCategory: "entity_type"}
		res, err := getFeedClient().GetFeedSummary(ctx, fsr)
		if assert.Nil(t, err) {
			assert.Equal(t, test.expectedEntryCount, res.TotalEntries)
		}
		suite.deleteAllDocuments()
		suite.refreshIndices(persistence.IndexNameFeeds)
	}
}

func getFeedClient() event_feed_api.EventFeedServiceClient {
	if feedClient != nil {
		return feedClient
	}
	timeoutCtx, cancel := context.WithTimeout(context.Background(), time.Second*10)
	defer cancel()

	conn, err := connFactory.DialContext(timeoutCtx, "event-feed-service",
		cfg.HandlerEndpoints.EventFeed, grpc.WithBlock())
	if err != nil {
		log.WithFields(log.Fields{
			"err": err,
		}).Fatal(fmt.Sprintf("grpc dial error... could not get FeedServiceClient"))
	}

	feedClient = event_feed_api.NewEventFeedServiceClient(conn)
	return feedClient
}

func getEventClient() api.EventServiceClient {
	if eventClient != nil {
		return eventClient
	}

	conn, err := connFactory.DialContext(context.Background(), "event-service", cfg.ListenAddress())
	if err != nil {
		log.WithFields(log.Fields{
			"err": err,
		}).Fatal(fmt.Sprintf("grpc dial error... could not get EventServiceClient"))
	}

	eventClient := api.NewEventServiceClient(conn)
	return eventClient
}
