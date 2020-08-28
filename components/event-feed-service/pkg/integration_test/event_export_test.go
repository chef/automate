package integration_test

import (
	"context"
	"fmt"
	"io"
	"net"
	"testing"
	"time"

	"github.com/buger/jsonparser"
	api "github.com/chef/automate/api/interservice/event_feed"
	authzConstants "github.com/chef/automate/components/authz-service/constants"
	"github.com/chef/automate/components/event-feed-service/pkg/feed"
	"github.com/chef/automate/components/event-feed-service/pkg/persistence"
	"github.com/chef/automate/components/event-feed-service/pkg/server"
	event "github.com/chef/automate/components/event-service/config"
	"github.com/chef/automate/lib/grpc/auth_context"
	"github.com/golang/protobuf/ptypes"
	"github.com/golang/protobuf/ptypes/timestamp"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
	"google.golang.org/grpc"
	"google.golang.org/grpc/test/bufconn"
	olivere "gopkg.in/olivere/elastic.v6"
)

func TestEventExportMatchAllFields(t *testing.T) {
	mockConn := reportExportSetup(t)
	defer mockConn.Close()
	client := api.NewEventFeedServiceClient(mockConn)

	entry := feed.FeedEntry{
		ID:               "id",
		ProducerName:     "Fred",
		ProducerID:       "ProducerID",
		FeedType:         "event",
		EventType:        event.ScanJobUpdatedEventName,
		ActorID:          "urn:mycompany:user:fred",
		ActorObjectType:  "User",
		ActorName:        "Fred",
		Verb:             "update",
		ObjectID:         "urn:chef:compliance:scan-job",
		ObjectObjectType: "ObjectObjectType",
		ObjectName:       "profile",
		TargetID:         "urn:mycompany:environment:production",
		TargetObjectType: "Environment",
		TargetName:       "Production",
		Created:          time.Now().UTC(),
		ChefInfraServer:  "chefserver",
		ChefOrganization: "org1",
		ParentName:       "parentname",
		ParentID:         "parentid",
	}
	testSuite.feedBackend.CreateFeedEntry(&entry)

	testSuite.RefreshIndices(persistence.IndexNameFeeds)

	defer testSuite.DeleteAllDocuments()

	response, err := client.EventExport(context.Background(), &api.EventExportRequest{})
	assert.NoError(t, err)
	require.NotNil(t, response)

	data := make([]byte, 0)
	for {
		tdata, err := response.Recv()
		if err != nil && err == io.EOF {
			data = append(data, tdata.GetContent()...)
			break
		}

		require.NoError(t, err)
		data = append(data, tdata.GetContent()...)
	}

	eventFound := false
	jsonparser.ArrayEach(data, func(event []byte, _ jsonparser.ValueType, _ int, err error) {
		require.NoError(t, err)
		eventFound = true
		eventType, err := jsonparser.GetString(event, "event_type")
		assert.Equal(t, "profile", eventType)

		ID, err := jsonparser.GetString(event, "id")
		assert.Equal(t, "id", ID)

		task, err := jsonparser.GetString(event, "task")
		assert.Equal(t, "update", task)

		requestorName, err := jsonparser.GetString(event, "requestor_name")
		assert.Equal(t, "Fred", requestorName)

		requestorType, err := jsonparser.GetString(event, "requestor_type")
		assert.Equal(t, "User", requestorType)

		org, err := jsonparser.GetString(event, "chef_organization")
		assert.Equal(t, "org1", org)

		server, err := jsonparser.GetString(event, "chef_infra_server")
		assert.Equal(t, "chefserver", server)

		parentName, err := jsonparser.GetString(event, "parent_name")
		assert.Equal(t, "parentname", parentName)

		parentID, err := jsonparser.GetString(event, "parent_id")
		assert.Equal(t, "parentid", parentID)
	})

	assert.True(t, eventFound, "The event was not returned")
}

func TestEventExportProjectFilters(t *testing.T) {
	mockConn := reportExportSetup(t)
	defer mockConn.Close()
	client := api.NewEventFeedServiceClient(mockConn)

	cases := []struct {
		description     string
		eventProjects   []string
		allowedProjects []string
		eventIsReturned bool
	}{
		{
			description:     "The Event is returned when the one event project matches the one allowed project",
			eventProjects:   []string{"one"},
			allowedProjects: []string{"one"},
			eventIsReturned: true,
		},
		{
			description:     "No events are returned when the one event project does not match the one allowed project",
			eventProjects:   []string{"one"},
			allowedProjects: []string{"two"},
			eventIsReturned: false,
		},
		{
			description:     "The event is returned when the one event project matches one of the two allowed projects",
			eventProjects:   []string{"one"},
			allowedProjects: []string{"one", "two"},
			eventIsReturned: true,
		},
		{
			description:     "The event is returned when both of the two event projects match both of the allowed projects",
			eventProjects:   []string{"one", "two"},
			allowedProjects: []string{"one", "two"},
			eventIsReturned: true,
		},
		{
			description:     "The event is returned when the event does not have an assigned project and the allowed project is unassigned",
			eventProjects:   []string{},
			allowedProjects: []string{authzConstants.UnassignedProjectID},
			eventIsReturned: true,
		},
		{
			description:     "The event is returned when the event does not have an assigned project and no allowed project are provided",
			eventProjects:   []string{},
			allowedProjects: []string{},
			eventIsReturned: true,
		},
		{
			description:     "No event is returned when the event does not have an assigned project and one allowed project",
			eventProjects:   []string{},
			allowedProjects: []string{"one"},
			eventIsReturned: false,
		},
		{
			description:     "No event is returned when the event has one project and the allowed project is unassigned",
			eventProjects:   []string{"one"},
			allowedProjects: []string{authzConstants.UnassignedProjectID},
			eventIsReturned: false,
		},
		{
			description:     "The event is returned when the event has a matching project with one of the allowed project with unassigned",
			eventProjects:   []string{"one"},
			allowedProjects: []string{authzConstants.UnassignedProjectID, "one"},
			eventIsReturned: true,
		},
		{
			description:     "The event is returned when the event has one project and all projects are allowed",
			eventProjects:   []string{"one"},
			allowedProjects: []string{authzConstants.AllProjectsExternalID},
			eventIsReturned: true,
		},
		{
			description:     "The event is returned when the event does not have a project and all projects are allowed",
			eventProjects:   []string{},
			allowedProjects: []string{authzConstants.AllProjectsExternalID},
			eventIsReturned: true,
		},
		{
			description:     "The event is returned when the event has a matching project with one of several allowed projects",
			eventProjects:   []string{"project7"},
			allowedProjects: []string{"project3", "project9", "project7", "project6"},
			eventIsReturned: true,
		},
		{
			description:     "No event is returned when the one event's project does not match several allowed projects",
			eventProjects:   []string{"project12"},
			allowedProjects: []string{"project3", "project9", "project7", "project6"},
			eventIsReturned: false,
		},
		{
			description:     "The event is returned when one of several event's projects matches the one project allowed",
			eventProjects:   []string{"project3", "project9", "project7", "project6"},
			allowedProjects: []string{"project3"},
			eventIsReturned: true,
		},
		{
			description:     "No event is returned when none of the several event's projects match the one allowed project",
			eventProjects:   []string{"project3", "project9", "project7", "project6"},
			allowedProjects: []string{"project12"},
			eventIsReturned: false,
		},
		{
			description:     "The event is returned when one of the several event's projects matches one of the several allowed projects",
			eventProjects:   []string{"project3", "project9", "project7", "project6"},
			allowedProjects: []string{"project1", "project2", "project7", "project12"},
			eventIsReturned: true,
		},
		{
			description:     "No event is returned when none of the several event's projects matches none of the several allowed projects",
			eventProjects:   []string{"project3", "project9", "project7", "project6"},
			allowedProjects: []string{"project1", "project2", "project11", "project12"},
			eventIsReturned: false,
		},
		{
			description:     "The event is returned when several of the event's projects match all of the several allowed projects",
			eventProjects:   []string{"project3", "project9", "project7", "project6"},
			allowedProjects: []string{"project3", "project9", "project7", "project6"},
			eventIsReturned: true,
		},
	}

	for _, test := range cases {
		t.Run(fmt.Sprintf("filter: %s", test.description), func(t *testing.T) {

			entry := feed.FeedEntry{
				ID:                 "id",
				ProducerName:       "Fred",
				ProducerID:         "environment",
				FeedType:           "event",
				EventType:          event.ScanJobUpdatedEventName,
				ActorID:            "urn:mycompany:user:fred",
				ActorObjectType:    "User",
				ActorName:          "Fred",
				Verb:               "update",
				ObjectID:           "urn:chef:compliance:scan-job",
				ObjectObjectType:   "profile",
				ObjectName:         "Scan Job",
				TargetID:           "urn:mycompany:environment:production",
				TargetObjectType:   "Environment",
				TargetName:         "Production",
				Created:            time.Now().UTC(),
				ProducerObjectType: "chef_server",
				Projects:           test.eventProjects,
			}

			testSuite.feedBackend.CreateFeedEntry(&entry)

			testSuite.RefreshIndices(persistence.IndexNameFeeds)

			defer testSuite.DeleteAllDocuments()

			ctx := auth_context.NewOutgoingContext(contextWithProjects(test.allowedProjects))
			response, err := client.EventExport(ctx, &api.EventExportRequest{
				OutputType: "json",
			})
			assert.NoError(t, err)
			require.NotNil(t, response)

			data := make([]byte, 0)
			for {
				tdata, err := response.Recv()
				if err != nil && err == io.EOF {
					data = append(data, tdata.GetContent()...)
					break
				}

				require.NoError(t, err)
				data = append(data, tdata.GetContent()...)
			}

			actualNumberOfEvents := 0
			jsonparser.ArrayEach(data, func(event []byte, _ jsonparser.ValueType, _ int, err error) {
				require.NoError(t, err)
				actualNumberOfEvents++
			})

			if test.eventIsReturned {
				assert.Equal(t, 1, actualNumberOfEvents,
					"One event should have been returned not %d", actualNumberOfEvents)
			} else {
				assert.Equal(t, 0, actualNumberOfEvents,
					"No events should have been returned instead %d was retuned", actualNumberOfEvents)
			}
		})
	}
}

func TestEventExportFilters(t *testing.T) {
	mockConn := reportExportSetup(t)
	defer mockConn.Close()
	client := api.NewEventFeedServiceClient(mockConn)

	cases := []struct {
		description    string
		entries        []feed.FeedEntry
		eventExport    *api.EventExportRequest
		expectedRunIDs []string
	}{
		{
			description: "One event added and returned with no filters",
			entries: []feed.FeedEntry{
				{
					ID:        "1",
					Published: time.Now().Add(-time.Minute),
				},
			},
			eventExport:    &api.EventExportRequest{},
			expectedRunIDs: []string{"1"},
		},
		{
			description: "Two events added and returned with no filters",
			entries: []feed.FeedEntry{
				{
					ID:        "1",
					Published: time.Now().Add(-time.Minute),
				},
				{
					ID:        "2",
					Published: time.Now().Add(-time.Minute),
				},
			},
			eventExport:    &api.EventExportRequest{},
			expectedRunIDs: []string{"1", "2"},
		},
		{
			description: "One event added and returned with date filters including the event",
			entries: []feed.FeedEntry{
				{
					ID:        "1",
					Published: time.Now().Add(-time.Minute),
				},
			},
			eventExport: &api.EventExportRequest{
				Start: time.Now().Add(-time.Hour).Unix() * 1000,
				End:   time.Now().Add(time.Hour).Unix() * 1000,
			},
			expectedRunIDs: []string{"1"},
		},
		{
			description: "One event added and zero returned with date filters excluding",
			entries: []feed.FeedEntry{
				{
					ID:        "1",
					Published: time.Now().Add(-time.Hour * 12),
				},
			},
			eventExport: &api.EventExportRequest{
				Start: time.Now().Add(-time.Hour).Unix() * 1000,
				End:   time.Now().Add(time.Hour).Unix() * 1000,
			},
			expectedRunIDs: []string{},
		},
		{
			description: "Two events added and one returned with date filters including one",
			entries: []feed.FeedEntry{
				{
					ID:        "1",
					Published: time.Now().Add(-time.Hour * 12),
				},
				{
					ID:        "2",
					Published: time.Now().Add(-time.Minute),
				},
			},
			eventExport: &api.EventExportRequest{
				Start: time.Now().Add(-time.Hour).Unix() * 1000,
				End:   time.Now().Add(time.Hour).Unix() * 1000,
			},
			expectedRunIDs: []string{"2"},
		},
		{
			description: "One event added and one returned with event-type filter including",
			entries: []feed.FeedEntry{
				{
					ID:               "1",
					Published:        time.Now().Add(-time.Hour),
					ObjectObjectType: "scanjobs",
				},
			},
			eventExport: &api.EventExportRequest{
				Filter: []string{"event-type:scanjobs"},
			},
			expectedRunIDs: []string{"1"},
		},
		{
			description: "Two events added and one returned with event-type filter including only one",
			entries: []feed.FeedEntry{
				{
					ID:               "1",
					Published:        time.Now().Add(-time.Hour),
					ObjectObjectType: "scanjobs",
				},
				{
					ID:               "2",
					Published:        time.Now().Add(-time.Hour),
					ObjectObjectType: "profile",
				},
			},
			eventExport: &api.EventExportRequest{
				Filter: []string{"event-type:scanjobs"},
			},
			expectedRunIDs: []string{"1"},
		},

		// task filter
		{
			description: "One event added and one returned with task filter including",
			entries: []feed.FeedEntry{
				{
					ID:        "1",
					Published: time.Now().Add(-time.Hour),
					Verb:      "update",
				},
			},
			eventExport: &api.EventExportRequest{
				Filter: []string{"task:update"},
			},
			expectedRunIDs: []string{"1"},
		},
		{
			description: "Two events added and one returned with task filter including only one",
			entries: []feed.FeedEntry{
				{
					ID:        "1",
					Published: time.Now().Add(-time.Hour),
					Verb:      "update",
				},
				{
					ID:        "2",
					Published: time.Now().Add(-time.Hour),
					Verb:      "create",
				},
			},
			eventExport: &api.EventExportRequest{
				Filter: []string{"task:update"},
			},
			expectedRunIDs: []string{"1"},
		},

		// requestor_name filter
		{
			description: "One event added and one returned with requestor_name filter including",
			entries: []feed.FeedEntry{
				{
					ID:        "1",
					Published: time.Now().Add(-time.Hour),
					ActorName: "bob",
				},
			},
			eventExport: &api.EventExportRequest{
				Filter: []string{"requestor_name:bob"},
			},
			expectedRunIDs: []string{"1"},
		},
		{
			description: "Two events added and one returned with requestor_name filter including only one",
			entries: []feed.FeedEntry{
				{
					ID:        "1",
					Published: time.Now().Add(-time.Hour),
					ActorName: "bob",
				},
				{
					ID:        "2",
					Published: time.Now().Add(-time.Hour),
					ActorName: "tim",
				},
			},
			eventExport: &api.EventExportRequest{
				Filter: []string{"requestor_name:bob"},
			},
			expectedRunIDs: []string{"1"},
		},

		// chef_server filter
		{
			description: "One event added and one returned with chef_server filter including",
			entries: []feed.FeedEntry{
				{
					ID:              "1",
					Published:       time.Now().Add(-time.Hour),
					ChefInfraServer: "chef3",
				},
			},
			eventExport: &api.EventExportRequest{
				Filter: []string{"chef_server:chef3"},
			},
			expectedRunIDs: []string{"1"},
		},
		{
			description: "Two events added and one returned with chef_server filter including only one",
			entries: []feed.FeedEntry{
				{
					ID:              "1",
					Published:       time.Now().Add(-time.Hour),
					ChefInfraServer: "chef3",
				},
				{
					ID:              "2",
					Published:       time.Now().Add(-time.Hour),
					ChefInfraServer: "chef2",
				},
			},
			eventExport: &api.EventExportRequest{
				Filter: []string{"chef_server:chef3"},
			},
			expectedRunIDs: []string{"1"},
		},

		// chef_organization filter
		{
			description: "One event added and one returned with chef_organization filter including",
			entries: []feed.FeedEntry{
				{
					ID:               "1",
					Published:        time.Now().Add(-time.Hour),
					ChefOrganization: "org1",
				},
			},
			eventExport: &api.EventExportRequest{
				Filter: []string{"chef_organization:org1"},
			},
			expectedRunIDs: []string{"1"},
		},
		{
			description: "Two events added and one returned with chef_organization filter including only one",
			entries: []feed.FeedEntry{
				{
					ID:               "1",
					Published:        time.Now().Add(-time.Hour),
					ChefOrganization: "org1",
				},
				{
					ID:               "2",
					Published:        time.Now().Add(-time.Hour),
					ChefOrganization: "org2",
				},
			},
			eventExport: &api.EventExportRequest{
				Filter: []string{"chef_organization:org1"},
			},
			expectedRunIDs: []string{"1"},
		},
	}

	for _, test := range cases {
		t.Run(fmt.Sprintf("filter: %s", test.description), func(t *testing.T) {

			for index := range test.entries {
				test.entries[index].ProducerName = "Fred"
				test.entries[index].ProducerID = "environment"
				test.entries[index].ProducerTags = []string{"mycompany", "engineering department", "compliance team"}
				test.entries[index].FeedType = "event"
				test.entries[index].EventType = event.ScanJobUpdatedEventName
				test.entries[index].ActorID = "urn:mycompany:user:fred"
				test.entries[index].ActorObjectType = "User"
				if test.entries[index].ActorName == "" {
					test.entries[index].ActorName = "Fred"
				}
				if test.entries[index].Verb == "" {
					test.entries[index].Verb = "update"
				}
				test.entries[index].ObjectID = "urn:chef:compliance:scan-job"
				if test.entries[index].ObjectObjectType == "" {
					test.entries[index].ObjectObjectType = "profile"
				}
				test.entries[index].ObjectName = "Scan Job"
				test.entries[index].TargetID = "urn:mycompany:environment:production"
				test.entries[index].TargetObjectType = "Environment"
				test.entries[index].TargetName = "Production"
				test.entries[index].Created = time.Now().UTC()

				testSuite.feedBackend.CreateFeedEntry(&test.entries[index])
			}

			testSuite.RefreshIndices(persistence.IndexNameFeeds)

			defer testSuite.DeleteAllDocuments()

			response, err := client.EventExport(context.Background(), test.eventExport)
			assert.NoError(t, err)
			require.NotNil(t, response)

			data := make([]byte, 0)
			for {
				tdata, err := response.Recv()
				if err != nil && err == io.EOF {
					data = append(data, tdata.GetContent()...)
					break
				}

				require.NoError(t, err)
				data = append(data, tdata.GetContent()...)
			}

			actualEventIDs := make([]string, 0)
			jsonparser.ArrayEach(data, func(event []byte, _ jsonparser.ValueType, _ int, err error) {
				require.NoError(t, err)
				eventID, err := jsonparser.GetString(event, "id")
				require.NoError(t, err)
				actualEventIDs = append(actualEventIDs, eventID)
			})

			assert.ElementsMatch(t, test.expectedRunIDs, actualEventIDs)
		})
	}
}

func reportExportSetup(t *testing.T) *grpc.ClientConn {
	esClient, _ := olivere.NewClient(
		olivere.SetURL(testSuite.elasticsearchUrl),
		olivere.SetSniff(false),
	)

	feedBackend := persistence.NewFeedStore(esClient)
	eventFeedServer := server.New(feedBackend)

	lis := bufconn.Listen(1024 * 1024)
	s := grpc.NewServer()
	api.RegisterEventFeedServiceServer(s, eventFeedServer)

	go func() {
		if err := s.Serve(lis); err != nil {
			t.Fatalf("Server exited with error: %v", err)
		}
	}()

	dialer := func(string, time.Duration) (net.Conn, error) { return lis.Dial() }

	conn, err := grpc.DialContext(context.Background(), "bufnet", grpc.WithDialer(dialer), grpc.WithInsecure())
	require.NoError(t, err)

	return conn
}

func createTimestamp(t *testing.T, date time.Time) *timestamp.Timestamp {
	timestamp, err := ptypes.TimestampProto(time.Now())
	require.NoError(t, err)

	return timestamp
}
