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
	feed "github.com/chef/automate/components/compliance-service/api/automate-feed"
	"github.com/chef/automate/components/compliance-service/ingest/ingestic/mappings"
	"github.com/chef/automate/components/event-service/server"
)

var (
	feedClient  feed.FeedServiceClient
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

	fe := feed.FeedEntry{
		ID:                   "unknown; assigned at feed entry creation time",
		EventType:            event.Type.Name,
		FeedType:             "event",
		Tags:                 event.Tags,
		SourceEventPublished: pubTs,
		Producer: &feed.Producer{
			ID:         event.Producer.ID,
			Name:       event.Producer.ProducerName,
			ObjectType: event.Producer.ProducerType,
			PTags:      event.Producer.Tags,
		},
		Actor: &feed.Actor{
			ID:         event.Actor.ID,
			Name:       event.Actor.DisplayName,
			ObjectType: event.Actor.ObjectType,
		},
		Verb: event.Verb,
		Object: &feed.Object{
			ID:         event.Object.ID,
			Name:       event.Object.DisplayName,
			ObjectType: event.Object.ObjectType,
		},
		Target: &feed.Target{
			ID:         event.Target.ID,
			ObjectType: event.Object.ObjectType,
			Name:       event.Target.DisplayName,
		},
	}

	entries := []*feed.FeedEntry{&fe}
	fr := feed.FeedResponse{TotalEntries: 1, FeedEntries: entries}

	req := api.PublishRequest{Msg: &event}

	tests := []struct {
		name string
		in   *api.PublishRequest
		out  bool
		res  *feed.FeedResponse
		e    *feed.FeedEntry
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
			suite.refreshIndices(mappings.IndexNameFeeds)

			feedResp, err := getFeedClient().GetFeed(ctx, &feed.FeedRequest{Size: 10})
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
		suite.refreshIndices(mappings.IndexNameFeeds)
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
		suite.refreshIndices(mappings.IndexNameFeeds)

		fsr := &feed.FeedSummaryRequest{CountCategory: "entity_type"}
		res, err := getFeedClient().GetFeedSummary(ctx, fsr)
		if assert.Nil(t, err) {
			assert.Equal(t, test.expectedEntryCount, res.TotalEntries)
		}
		suite.deleteAllDocuments()
		suite.refreshIndices(mappings.IndexNameFeeds)
	}
}

func getFeedClient() feed.FeedServiceClient {
	if feedClient != nil {
		return feedClient
	}

	conn, err := connFactory.DialContext(context.Background(), "compliance-service", cfg.HandlerEndpoints.Feed, grpc.WithBlock())
	if err != nil {
		log.WithFields(log.Fields{
			"err": err,
		}).Fatal(fmt.Sprintf("grpc dial error... could not get FeedServiceClient"))
	}

	feedClient = feed.NewFeedServiceClient(conn)
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
