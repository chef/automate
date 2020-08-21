//
//  Author:: Salim Afiune <afiune@chef.io>, Gina Peers <gpeers@chef.io>
//  Copyright:: Copyright 2018, Chef Software Inc.
//

package integration_test

import (
	"context"
	"fmt"
	"testing"
	"time"

	"github.com/gofrs/uuid"
	google_protobuf "github.com/golang/protobuf/ptypes/timestamp"
	"github.com/stretchr/testify/assert"

	event "github.com/chef/automate/components/event-service/config"

	"github.com/chef/automate/api/interservice/event_feed"
	"github.com/chef/automate/components/event-feed-service/pkg/feed"
	"github.com/chef/automate/components/event-feed-service/pkg/persistence"
	"github.com/chef/automate/components/event-feed-service/pkg/server"

	log "github.com/sirupsen/logrus"
	"google.golang.org/grpc/codes"

	authzConstants "github.com/chef/automate/components/authz-service/constants"
	"github.com/chef/automate/lib/grpc/auth_context"
	"github.com/chef/automate/lib/grpc/grpctest"
)

var (
	millisPerSecond     = int64(1000)
	nanosPerMillisecond = int32(time.Millisecond)
)

func TestEventFeedReturnErrorWithWrongParameters(t *testing.T) {
	var (
		ctx  = context.Background()
		date = time.Now().UTC()
	)
	cases := []struct {
		description string
		request     event_feed.FeedRequest
	}{
		{
			description: "The Start date is after the End date",
			request: event_feed.FeedRequest{
				End:   date.AddDate(0, 0, -6).Unix() * 1000,
				Start: date.Unix() * 1000,
				Size:  10,
			},
		},
		{
			description: "Before and After parameters should not both be set",
			request: event_feed.FeedRequest{
				End:    date.Unix() * 1000,
				Start:  date.AddDate(0, 0, -6).Unix() * 1000,
				Size:   10,
				Before: date.AddDate(0, 0, -3).Unix() * 1000,
				After:  date.AddDate(0, 0, -1).Unix() * 1000,
			},
		},
		{
			description: "If the Before param is set the Cursor param needs to be set also",
			request: event_feed.FeedRequest{
				End:    date.Unix() * 1000,
				Start:  date.AddDate(0, 0, -6).Unix() * 1000,
				Size:   10,
				Before: date.AddDate(0, 0, -3).Unix() * 1000,
			},
		},
		{
			description: "When the After is set without the Cursor, After must be equal to End",
			request: event_feed.FeedRequest{
				End:   date.Unix() * 1000,
				Start: date.AddDate(0, 0, -6).Unix() * 1000,
				Size:  10,
				After: date.AddDate(0, 0, -3).Unix() * 1000,
			},
		},
	}

	for _, test := range cases {
		t.Run(fmt.Sprintf("with parameters filters=%v it %s should return an error",
			test.request, test.description), func(t *testing.T) {

			_, err := testSuite.feedClient.GetFeed(ctx, &test.request)

			grpctest.AssertCode(t, codes.InvalidArgument, err)
		})
	}
}

func TestEventFeedReturnOnlyEventsWithinDateRange(t *testing.T) {
	var (
		ctx          = context.Background()
		totalEntries = 10
		pageSize     = int32(totalEntries)
		startDate    = time.Now().UTC()
		entries      = []*feed.FeedEntry{}
	)

	for i := 0; i < totalEntries; i++ {

		var (
			published = startDate.AddDate(0, 0, i*-1)
			created   = published
		)

		data := feed.FeedEntry{
			ID:                 uuid.Must(uuid.NewV4()).String(),
			ProducerID:         "urn:mycompany:user:violet",
			ProducerName:       "Violet",
			ProducerObjectType: "user",
			ProducerTags:       []string{"mycompany", "engineering department", "compliance team"},
			FeedType:           "event",
			EventType:          event.ScanJobUpdatedEventName,
			Tags:               []string{"mygroup", "compliance", "scan"},
			Published:          published,
			ActorID:            "urn:mycompany:user:violet",
			ActorObjectType:    "User",
			ActorName:          "Violet",
			Verb:               "update",
			ObjectID:           "urn:chef:compliance:scan-job",
			ObjectObjectType:   "ScanJob",
			ObjectName:         "Scan Job",
			TargetID:           "urn:mycompany:environment:production",
			TargetObjectType:   "Environment",
			TargetName:         "Production",
			Created:            created,
		}
		entries = append(entries, &data)
		// TODO: write bulk load
		testSuite.feedBackend.CreateFeedEntry(&data)
	}

	testSuite.RefreshIndices(persistence.IndexNameFeeds)
	defer GetTestSuite().DeleteAllDocuments()

	expectedEntries, err := server.FromInternalFormatToList(entries)
	if err != nil {
		log.Warnf("Couldn't set up expected result set... error converting feed entries from internal format to rpc; error: %v", err)
	}

	cases := []struct {
		description string
		request     event_feed.FeedRequest
		expected    []*event_feed.FeedEntry
	}{
		{
			description: "should return all 10 events (default)",
			request: event_feed.FeedRequest{
				Size: pageSize,
			},
			expected: expectedEntries,
		},
		{
			description: "should return all 10 events",
			request: event_feed.FeedRequest{
				Start: startDate.AddDate(0, 0, -10).Unix() * 1000,
				Size:  pageSize,
			},
			expected: expectedEntries,
		},
		{
			description: "should return first 5 events",
			request: event_feed.FeedRequest{
				Start: startDate.AddDate(0, 0, -5).Unix() * 1000,
				Size:  pageSize,
			},
			expected: expectedEntries[0:6],
		},
		{
			description: "should return last 5 events",
			request: event_feed.FeedRequest{
				End:  startDate.AddDate(0, 0, -5).Unix() * 1000,
				Size: pageSize,
			},
			expected: expectedEntries[5:10],
		},
		{
			description: "should return one event",
			request: event_feed.FeedRequest{
				Start: startDate.AddDate(0, 0, -5).Unix() * 1000,
				End:   startDate.AddDate(0, 0, -5).Unix() * 1000,
				Size:  pageSize,
			},
			expected: expectedEntries[5:6],
		},
		{
			description: "dates after should return zero events",
			request: event_feed.FeedRequest{
				Start: startDate.AddDate(0, 0, 1).Unix() * 1000,
				End:   startDate.AddDate(0, 0, 5).Unix() * 1000,
				Size:  pageSize,
			},
			expected: []*event_feed.FeedEntry{},
		},
		{
			description: "dates before should return zero events",
			request: event_feed.FeedRequest{
				Start: startDate.AddDate(0, 0, -11).Unix() * 1000,
				End:   startDate.AddDate(0, 0, -10).Unix() * 1000,
				Size:  pageSize,
			},
			expected: []*event_feed.FeedEntry{},
		},
	}

	// Run all the cases!
	for _, test := range cases {
		t.Run(fmt.Sprintf("with request '%v' it %s", test.request, test.description),
			func(t *testing.T) {
				res, err := testSuite.feedClient.GetFeed(ctx, &test.request)
				if assert.Nil(t, err) {
					assert.Equal(t, len(test.expected), len(res.FeedEntries))
					for index, expectedEvent := range test.expected {
						assert.Equal(t, expectedEvent.SourceEventPublished, res.FeedEntries[index].SourceEventPublished)
					}
				}
			})
	}
}

func TestEventFeedFilterEventType(t *testing.T) {
	var (
		ctx          = context.Background()
		totalEntries = 12
		pageSize     = int32(totalEntries)
		entries      = []*feed.FeedEntry{}
		eventTypes   = []string{"profile", "scanjobs"}
	)

	for i := 0; i < totalEntries; i++ {
		var (
			name             = "Fred"
			userURN          = "urn:mycompany:user:fred"
			eventType        = event.ScanJobUpdatedEventName
			tags             = []string{"org_1", "compliance", eventTypes[1]}
			verb             = "update"
			objectID         = "urn:chef:compliance:scan-job"
			objectObjectType = eventTypes[1]
			objectName       = "Scan Job"
		)

		if i > 5 {
			name = "Violet"
			userURN = "urn:mycompany:user:violet"
			eventType = event.ProfileCreatedEventName
			tags = []string{"org_2", "compliance", eventTypes[0]}
			verb = "create"
			objectID = "urn:chef:compliance:profile"
			objectObjectType = eventTypes[0]
			objectName = "Profile"
		}

		data := feed.FeedEntry{
			ID:                 uuid.Must(uuid.NewV4()).String(),
			ProducerID:         userURN,
			ProducerName:       name,
			ProducerObjectType: "user",
			ProducerTags:       []string{"mycompany", "engineering department", "compliance team"},
			FeedType:           "event",
			EventType:          eventType,
			Tags:               tags,
			Published:          time.Now().UTC(),
			ActorID:            userURN,
			ActorObjectType:    "User",
			ActorName:          name,
			Verb:               verb,
			ObjectID:           objectID,
			ObjectObjectType:   objectObjectType,
			ObjectName:         objectName,
			TargetID:           "urn:mycompany:environment:production",
			TargetObjectType:   "Environment",
			TargetName:         "Production",
			Created:            time.Now().UTC(),
		}

		entries = append(entries, &data)
		// TODO: write bulk load
		testSuite.feedBackend.CreateFeedEntry(&data)
	}

	testSuite.RefreshIndices(persistence.IndexNameFeeds)
	defer testSuite.DeleteAllDocuments()

	expectedEntries, err := server.FromInternalFormatToList(entries)
	if err != nil {
		log.Warnf("Couldn't set up expected result set... error converting feed entries from internal format to rpc; error: %v", err)
		expectedEntries = []*event_feed.FeedEntry{}
	}

	// reverse the list of expected entries to put them in desc order
	for i, j := 0, len(expectedEntries)-1; i < j; i, j = i+1, j-1 {
		expectedEntries[i], expectedEntries[j] = expectedEntries[j], expectedEntries[i]
	}

	cases := []struct {
		description string
		request     event_feed.FeedRequest
		expected    []*event_feed.FeedEntry
	}{
		{
			description: "should return all 12 events (default)",
			request: event_feed.FeedRequest{
				Size: pageSize,
			},
			expected: expectedEntries,
		},
		{
			description: "should return only 6 profile type events",
			request: event_feed.FeedRequest{
				Filters: []string{"event-type:" + eventTypes[0]},
				Size:    pageSize,
			},
			expected: expectedEntries[0:6],
		},
		{
			description: "should return only 6 scan job type events",
			request: event_feed.FeedRequest{
				Filters: []string{"event-type:" + eventTypes[1]},
				Size:    pageSize,
			},
			expected: expectedEntries[6:12],
		},
		{
			description: "filter requestor name of 'fred'",
			request: event_feed.FeedRequest{
				Filters: []string{"requestor_name:Fred"},
				Size:    pageSize,
			},
			expected: expectedEntries[6:12],
		},
		{
			description: "filter requestor name of 'Violet'",
			request: event_feed.FeedRequest{
				Filters: []string{"requestor_name:Violet"},
				Size:    pageSize,
			},
			expected: expectedEntries[0:6],
		},
	}

	// Run all the cases!
	for _, test := range cases {
		t.Run(fmt.Sprintf("with request '%v' it %s", test.request, test.description),
			func(t *testing.T) {
				res, err := testSuite.feedClient.GetFeed(ctx, &test.request)
				if assert.Nil(t, err) {
					assert.Equal(t, int64(len(test.expected)), res.TotalEntries)
					assert.ElementsMatch(t, test.expected, res.FeedEntries)
				}
			})
	}
}

func TestEventFeedProjectFilter(t *testing.T) {
	request := &event_feed.FeedRequest{
		Size: 100,
	}
	cases := []struct {
		description string
		entries     []feed.FeedEntry
		ctx         context.Context
		expectedIDs []string
	}{
		{
			description: "No Events with requesting projects",
			entries:     []feed.FeedEntry{},
			ctx:         contextWithProjects([]string{"project9"}),
			expectedIDs: []string{},
		},
		{
			description: "One event returned. One chef_server event with a project. A matching project requested",
			entries: []feed.FeedEntry{
				{
					ID:                 "1",
					Projects:           []string{"project9"},
					ProducerObjectType: "chef_server",
				},
			},
			ctx:         contextWithProjects([]string{"project9"}),
			expectedIDs: []string{"1"},
		},
		{
			description: "One event returned. One non-chef_server event with no project. A request with projects",
			entries: []feed.FeedEntry{
				{
					ID:                 "1",
					ProducerObjectType: "profile",
				},
			},
			ctx:         contextWithProjects([]string{"project9"}),
			expectedIDs: []string{"1"},
		},
		{
			description: "No events returned. One chef_server event with a no projects. A non-matching project requested",
			entries: []feed.FeedEntry{
				{
					ID:                 "1",
					ProducerObjectType: "chef_server",
				},
			},
			ctx:         contextWithProjects([]string{"project9"}),
			expectedIDs: []string{},
		},
		{
			description: "No events returned. One chef_server event with a non-matching project with the request",
			entries: []feed.FeedEntry{
				{
					ID:                 "1",
					ProducerObjectType: "chef_server",
					Projects:           []string{"non-matching"},
				},
			},
			ctx:         contextWithProjects([]string{"project9"}),
			expectedIDs: []string{},
		},
		{
			description: "Two events returned. Two chef_server events with the same project. A matching project requested",
			entries: []feed.FeedEntry{
				{
					ID:                 "1",
					Projects:           []string{"project9"},
					ProducerObjectType: "chef_server",
				},
				{
					ID:                 "2",
					Projects:           []string{"project9"},
					ProducerObjectType: "chef_server",
				},
			},
			ctx:         contextWithProjects([]string{"project9"}),
			expectedIDs: []string{"1", "2"},
		},
		{
			description: "Two events returned. One non-chef_server and one chef_server event with a matching request project",
			entries: []feed.FeedEntry{
				{
					ID:                 "1",
					ProducerObjectType: "profile",
				},
				{
					ID:                 "2",
					Projects:           []string{"project9"},
					ProducerObjectType: "chef_server",
				},
			},
			ctx:         contextWithProjects([]string{"project9"}),
			expectedIDs: []string{"1", "2"},
		},
		{
			description: "One event returned. One non-chef_server and one chef_server event with a non-matching request project",
			entries: []feed.FeedEntry{
				{
					ID:                 "1",
					ProducerObjectType: "profile",
				},
				{
					ID:                 "2",
					Projects:           []string{"not-matching-project"},
					ProducerObjectType: "chef_server",
				},
			},
			ctx:         contextWithProjects([]string{"project9"}),
			expectedIDs: []string{"1"},
		},
		{
			description: "One event returned. Two chef_server events with only one project matching the request",
			entries: []feed.FeedEntry{
				{
					ID:                 "1",
					Projects:           []string{"project9"},
					ProducerObjectType: "chef_server",
				},
				{
					ID:                 "2",
					Projects:           []string{"not-matching"},
					ProducerObjectType: "chef_server",
				},
			},
			ctx:         contextWithProjects([]string{"project9"}),
			expectedIDs: []string{"1"},
		},
		{
			description: "One event returned. One chef_server event with a project. All projects requested",
			entries: []feed.FeedEntry{
				{
					ID:                 "1",
					Projects:           []string{"any-project-name"},
					ProducerObjectType: "chef_server",
				},
			},
			ctx:         contextWithProjects([]string{authzConstants.AllProjectsExternalID}),
			expectedIDs: []string{"1"},
		},
		{
			description: "Three events returned. Two chef_server events with different projects and the other with no projects. All projects requested",
			entries: []feed.FeedEntry{
				{
					ID:                 "1",
					Projects:           []string{"any-project-name"},
					ProducerObjectType: "chef_server",
				},
				{
					ID:                 "2",
					Projects:           []string{"another-any-project-name"},
					ProducerObjectType: "chef_server",
				},
				{
					ID:                 "3",
					ProducerObjectType: "chef_server",
				},
			},
			ctx:         contextWithProjects([]string{authzConstants.AllProjectsExternalID}),
			expectedIDs: []string{"1", "2", "3"},
		},
		{
			description: "One event returned. One chef_server event with a no project. All projects requested",
			entries: []feed.FeedEntry{
				{
					ID:                 "1",
					ProducerObjectType: "chef_server",
				},
			},
			ctx:         contextWithProjects([]string{authzConstants.AllProjectsExternalID}),
			expectedIDs: []string{"1"},
		},
		{
			description: "One event returned. One chef_server event with a no project. Unassigned projects requested",
			entries: []feed.FeedEntry{
				{
					ID:                 "1",
					ProducerObjectType: "chef_server",
				},
			},
			ctx:         contextWithProjects([]string{authzConstants.UnassignedProjectID}),
			expectedIDs: []string{"1"},
		},
		{
			description: "No events returned. One chef_server event with a project. Unassigned projects requested",
			entries: []feed.FeedEntry{
				{
					ID:                 "1",
					ProducerObjectType: "chef_server",
					Projects:           []string{"project9"},
				},
			},
			ctx:         contextWithProjects([]string{authzConstants.UnassignedProjectID}),
			expectedIDs: []string{},
		},
		{
			description: "One event returned. Two chef_server events one with a project and the other without a project. Unassigned projects requested",
			entries: []feed.FeedEntry{
				{
					ID:                 "1",
					ProducerObjectType: "chef_server",
					Projects:           []string{"project9"},
				},
				{
					ID:                 "2",
					ProducerObjectType: "chef_server",
				},
			},
			ctx:         contextWithProjects([]string{authzConstants.UnassignedProjectID}),
			expectedIDs: []string{"2"},
		},
		{
			description: "One event returned. One chef_server event with a project. Unassigned and one matching projects requested",
			entries: []feed.FeedEntry{
				{
					ID:                 "1",
					ProducerObjectType: "chef_server",
					Projects:           []string{"project9"},
				},
			},
			ctx:         contextWithProjects([]string{authzConstants.UnassignedProjectID, "project9"}),
			expectedIDs: []string{"1"},
		},
		{
			description: "One event returned. One chef_server event without a project. No projects requested",
			entries: []feed.FeedEntry{
				{
					ID:                 "1",
					ProducerObjectType: "chef_server",
					Projects:           []string{},
				},
			},
			ctx:         contextWithProjects([]string{}),
			expectedIDs: []string{"1"},
		},
		{
			description: "Two events returned. Both are chef_server events with and without projects. No projects requested",
			entries: []feed.FeedEntry{
				{
					ID:                 "1",
					ProducerObjectType: "chef_server",
					Projects:           []string{},
				},
				{
					ID:                 "2",
					ProducerObjectType: "chef_server",
					Projects:           []string{"project9"},
				},
			},
			ctx:         contextWithProjects([]string{}),
			expectedIDs: []string{"1", "2"},
		},
		{
			description: "One event returned. One chef_server event with a project. One matching of several requested projects",
			entries: []feed.FeedEntry{
				{
					ID:                 "1",
					ProducerObjectType: "chef_server",
					Projects:           []string{"project9"},
				},
			},
			ctx:         contextWithProjects([]string{"project3", "project9", "project7", "project6"}),
			expectedIDs: []string{"1"},
		},
		{
			description: "Two events returned. Two chef_server events with different projects. Two matching projects of several requested",
			entries: []feed.FeedEntry{
				{
					ID:                 "1",
					ProducerObjectType: "chef_server",
					Projects:           []string{"project9"},
				},
				{
					ID:                 "2",
					ProducerObjectType: "chef_server",
					Projects:           []string{"project3"},
				},
			},
			ctx:         contextWithProjects([]string{"project3", "project9", "project7", "project6"}),
			expectedIDs: []string{"1", "2"},
		},
		{
			description: "No events returned. One chef_server event a projects. No matching projects of several requested",
			entries: []feed.FeedEntry{
				{
					ID:                 "1",
					ProducerObjectType: "chef_server",
					Projects:           []string{"project9"},
				},
			},
			ctx:         contextWithProjects([]string{"project3", "project8", "project7", "project6"}),
			expectedIDs: []string{},
		},
		{
			description: "No events returned. Two chef_server events different projects. No matching projects of several requested",
			entries: []feed.FeedEntry{
				{
					ID:                 "1",
					ProducerObjectType: "chef_server",
					Projects:           []string{"project9"},
				},
				{
					ID:                 "2",
					ProducerObjectType: "chef_server",
					Projects:           []string{"project10"},
				},
			},
			ctx:         contextWithProjects([]string{"project3", "project8", "project7", "project6"}),
			expectedIDs: []string{},
		},
		{
			description: "One event returned. One chef_server event with several different projects. One matching project requested",
			entries: []feed.FeedEntry{
				{
					ID:                 "1",
					ProducerObjectType: "chef_server",
					Projects:           []string{"project3", "project4", "project7", "project6"},
				},
			},
			ctx:         contextWithProjects([]string{"project3"}),
			expectedIDs: []string{"1"},
		},
		{
			description: "Two events returned. Two chef_server events both with several projects. One project matching both events requested",
			entries: []feed.FeedEntry{
				{
					ID:                 "1",
					ProducerObjectType: "chef_server",
					Projects:           []string{"project5", "project6", "project3", "project7"},
				},
				{
					ID:                 "2",
					ProducerObjectType: "chef_server",
					Projects:           []string{"project1", "project3", "project2", "project4"},
				},
			},
			ctx:         contextWithProjects([]string{"project3"}),
			expectedIDs: []string{"1", "2"},
		},
		{
			description: "One event returned. Two chef_server events both with several projects. One project matching one of the events requested",
			entries: []feed.FeedEntry{
				{
					ID:                 "1",
					ProducerObjectType: "chef_server",
					Projects:           []string{"project5", "project6", "project3", "project7"},
				},
				{
					ID:                 "2",
					ProducerObjectType: "chef_server",
					Projects:           []string{"project1", "project3", "project2", "project4"},
				},
			},
			ctx:         contextWithProjects([]string{"project6"}),
			expectedIDs: []string{"1"},
		},
		{
			description: "One event returned. One chef_server event with several different projects. Several projects with one matching requested",
			entries: []feed.FeedEntry{
				{
					ID:                 "1",
					ProducerObjectType: "chef_server",
					Projects:           []string{"project3", "project4", "project7", "project6"},
				},
			},
			ctx:         contextWithProjects([]string{"project1", "project3", "project2", "project14"}),
			expectedIDs: []string{"1"},
		},
		{
			description: "Two events returned. Two chef_server events both with several projects. Several projects with different one matching the events requested",
			entries: []feed.FeedEntry{
				{
					ID:                 "1",
					ProducerObjectType: "chef_server",
					Projects:           []string{"project5", "project6", "project3", "project7"},
				},
				{
					ID:                 "2",
					ProducerObjectType: "chef_server",
					Projects:           []string{"project1", "project9", "project2", "project4"},
				},
			},
			ctx:         contextWithProjects([]string{"project11", "project9", "project6", "project14"}),
			expectedIDs: []string{"1", "2"},
		},
		{
			description: "One event returned. One chef_server event with several different projects. Several projects with non matching requested",
			entries: []feed.FeedEntry{
				{
					ID:                 "1",
					ProducerObjectType: "chef_server",
					Projects:           []string{"project3", "project4", "project7", "project6"},
				},
			},
			ctx:         contextWithProjects([]string{"project1", "project13", "project2", "project14"}),
			expectedIDs: []string{},
		},
		{
			description: "One event returned. One chef_server event with several different projects. Several projects with two matching requested",
			entries: []feed.FeedEntry{
				{
					ID:                 "1",
					ProducerObjectType: "chef_server",
					Projects:           []string{"project3", "project10", "project7", "project6"},
				},
			},
			ctx:         contextWithProjects([]string{"project3", "project10", "project12", "project13"}),
			expectedIDs: []string{"1"},
		},
	}

	for _, test := range cases {
		t.Run(fmt.Sprintf("Project filter: %s", test.description), func(t *testing.T) {
			for index := range test.entries {
				test.entries[index].Published = time.Now()
				test.entries[index].ProducerName = "Fred"
				test.entries[index].ProducerID = "environment"
				test.entries[index].ProducerTags = []string{"mycompany", "engineering department", "compliance team"}
				test.entries[index].FeedType = "event"
				test.entries[index].EventType = event.ScanJobUpdatedEventName
				test.entries[index].Tags = []string{"org_1", "compliance", "profile"}
				test.entries[index].ActorID = "urn:mycompany:user:fred"
				test.entries[index].ActorObjectType = "User"
				test.entries[index].ActorName = "Fred"
				test.entries[index].Verb = "update"
				test.entries[index].ObjectID = "urn:chef:compliance:scan-job"
				test.entries[index].ObjectObjectType = "profile"
				test.entries[index].ObjectName = "Scan Job"
				test.entries[index].TargetID = "urn:mycompany:environment:production"
				test.entries[index].TargetObjectType = "Environment"
				test.entries[index].TargetName = "Production"
				test.entries[index].Created = time.Now().UTC()

				testSuite.feedBackend.CreateFeedEntry(&test.entries[index])
			}
			testSuite.RefreshIndices(persistence.IndexNameFeeds)

			defer testSuite.DeleteAllDocuments()

			res, err := testSuite.feedClient.GetFeed(test.ctx, request)
			assert.NoError(t, err)

			// collect IDs from events response
			eventIDs := make([]string, res.TotalEntries)
			for index, event := range res.FeedEntries {
				eventIDs[index] = event.Id
			}

			// test response
			assert.ElementsMatch(t, test.expectedIDs, eventIDs)
		})
	}
}

func TestEventFeedChefServerChefOrgFilters(t *testing.T) {

	cases := []struct {
		description string
		entries     []feed.FeedEntry
		filters     []string
		expectedIDs []string
	}{
		{
			description: "One event stored; filter for a chef infra server matching the event. One event is returned",
			entries: []feed.FeedEntry{
				{
					ID:                 "1",
					ChefInfraServer:    "server1",
					ProducerObjectType: "chef_server",
				},
			},
			filters:     []string{"chef_server:server1"},
			expectedIDs: []string{"1"},
		},
		{
			description: "One event stored; filter for a chef infra server non-matching the event; no events are returned",
			entries: []feed.FeedEntry{
				{
					ID:                 "1",
					ChefInfraServer:    "server1",
					ProducerObjectType: "chef_server",
				},
			},
			filters:     []string{"chef_server:server2"},
			expectedIDs: []string{},
		},
		{
			description: "Two events stored with different chef servers; filtering for only one of the events; One event is returned",
			entries: []feed.FeedEntry{
				{
					ID:                 "1",
					ChefInfraServer:    "server1",
					ProducerObjectType: "chef_server",
				},
				{
					ID:                 "2",
					ChefInfraServer:    "server2",
					ProducerObjectType: "chef_server",
				},
			},
			filters:     []string{"chef_server:server1"},
			expectedIDs: []string{"1"},
		},

		{
			description: "One event stored; filter for a chef org matching the event. One event is returned",
			entries: []feed.FeedEntry{
				{
					ID:                 "1",
					ChefOrganization:   "org1",
					ProducerObjectType: "chef_server",
				},
			},
			filters:     []string{"organization:org1"},
			expectedIDs: []string{"1"},
		},
		{
			description: "One event stored; filter for a chef org non-matching the event; no events are returned",
			entries: []feed.FeedEntry{
				{
					ID:                 "1",
					ChefOrganization:   "org1",
					ProducerObjectType: "chef_server",
				},
			},
			filters:     []string{"organization:org2"},
			expectedIDs: []string{},
		},
	}

	for _, test := range cases {
		t.Run(fmt.Sprintf("Project filter: %s", test.description), func(t *testing.T) {
			for index := range test.entries {
				test.entries[index].Published = time.Now()
				test.entries[index].ProducerName = "Fred"
				test.entries[index].ProducerID = "environment"
				test.entries[index].ProducerTags = []string{"mycompany", "engineering department", "compliance team"}
				test.entries[index].FeedType = "event"
				test.entries[index].EventType = event.ScanJobUpdatedEventName
				test.entries[index].Tags = []string{"org_1", "compliance", "profile"}
				test.entries[index].ActorID = "urn:mycompany:user:fred"
				test.entries[index].ActorObjectType = "User"
				test.entries[index].ActorName = "Fred"
				test.entries[index].Verb = "update"
				test.entries[index].ObjectID = "urn:chef:compliance:scan-job"
				test.entries[index].ObjectObjectType = "profile"
				test.entries[index].ObjectName = "Scan Job"
				test.entries[index].TargetID = "urn:mycompany:environment:production"
				test.entries[index].TargetObjectType = "Environment"
				test.entries[index].TargetName = "Production"
				test.entries[index].Created = time.Now().UTC()

				testSuite.feedBackend.CreateFeedEntry(&test.entries[index])
			}
			testSuite.RefreshIndices(persistence.IndexNameFeeds)

			defer testSuite.DeleteAllDocuments()

			request := &event_feed.FeedRequest{
				Size:    100,
				Filters: test.filters,
			}

			res, err := testSuite.feedClient.GetFeed(context.Background(), request)
			assert.NoError(t, err)

			// collect IDs from events response
			eventIDs := make([]string, res.TotalEntries)
			for index, event := range res.FeedEntries {
				eventIDs[index] = event.Id
			}

			// test response
			assert.ElementsMatch(t, test.expectedIDs, eventIDs)
		})
	}
}

func toMilliseconds(timestamp *google_protobuf.Timestamp) int64 {
	return timestamp.GetSeconds()*millisPerSecond +
		int64(timestamp.GetNanos()/nanosPerMillisecond)
}

func contextWithProjects(projects []string) context.Context {
	ctx := context.Background()
	return auth_context.NewContext(ctx, []string{}, projects, "", "")
}
