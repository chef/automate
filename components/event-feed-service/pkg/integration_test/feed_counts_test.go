//
//  Author:: Salim Afiune <afiune@chef.io>, Gina Peers <gpeers@chef.io>
//  Copyright:: Copyright 2018, Chef Software Inc.
//

package integration_test

import (
	"context"
	"testing"
	"time"

	"math/rand"
	"strconv"

	"fmt"

	"github.com/stretchr/testify/assert"
	"google.golang.org/grpc/codes"

	"github.com/chef/automate/api/interservice/event_feed"
	"github.com/chef/automate/components/event-feed-service/pkg/feed"
	"github.com/chef/automate/components/event-feed-service/pkg/persistence"
	"github.com/chef/automate/lib/grpc/grpctest"
	"github.com/chef/automate/lib/pcmp/passert"

	authzConstants "github.com/chef/automate/components/authz-service/constants"
	event "github.com/chef/automate/components/event-service/config"
	"github.com/chef/automate/lib/stringutils"
)

type testCase struct {
	description    string
	request        event_feed.FeedSummaryRequest
	expectedCounts *event_feed.FeedSummaryResponse
}

func TestFeedCountsReturnErrorWithWrongParameters(t *testing.T) {
	var (
		ctx  = context.Background()
		date = time.Now()
	)

	_, err := testSuite.feedClient.GetFeedSummary(ctx, &event_feed.FeedSummaryRequest{
		End:   date.AddDate(0, 0, -6).Unix() * 1000,
		Start: date.Unix() * 1000,
	})

	grpctest.AssertCode(t, codes.InvalidArgument, err)
}

func TestFeedCountsReturnOnlyEventsWithinDateRange(t *testing.T) {
	var (
		totalEntries = 10
		startDate    = time.Now().UTC()
		timeDiff     = int(time.Hour) * -24
		entries      = createEntries(startDate, totalEntries, timeDiff)
	)

	for _, entry := range entries {
		testSuite.feedBackend.CreateFeedEntry(entry)
	}

	testSuite.RefreshIndices(persistence.IndexNameFeeds)
	defer GetTestSuite().DeleteAllDocuments()

	expectedCounts := entriesToTypeCounts(entries)

	cases := []testCase{
		{
			description:    "should count 10 events (default)",
			request:        event_feed.FeedSummaryRequest{CountCategory: "event-type"},
			expectedCounts: expectedCounts,
		},
		{
			description: "should count all 10 events",
			request: event_feed.FeedSummaryRequest{
				CountCategory: "event-type",
				Start:         startDate.AddDate(0, 0, -11).Unix() * 1000,
			},
			expectedCounts: expectedCounts,
		},
		{
			description: "should count only first event",
			request: event_feed.FeedSummaryRequest{
				CountCategory: "event-type",
				End:           startDate.AddDate(0, 0, -1).Unix() * 1000,
			},
			expectedCounts: entriesToTypeCounts(entries[1:10]),
		},
		{
			description: "should count only one event",
			request: event_feed.FeedSummaryRequest{
				CountCategory: "event-type",
				Start:         startDate.AddDate(0, 0, -5).Unix() * 1000,
				End:           startDate.AddDate(0, 0, -5).Unix() * 1000,
			},
			expectedCounts: entriesToTypeCounts(entries[5:6]),
		},
	}

	// Run all the cases!
	runCases(t, cases)
}

func TestFeedCountsReturnCountOverAThousandActions(t *testing.T) {
	var (
		ctx          = context.Background()
		totalEntries = 1001
		startDate    = time.Now().UTC()
		timeDiff     = int(time.Minute) * -1
		entries      = createEntries(startDate, totalEntries, timeDiff)
		request      = event_feed.FeedSummaryRequest{
			CountCategory: "event-type",
			Start:         startDate.AddDate(0, 0, -11).Unix() * 1000,
		}
		expectedCounts = entriesToTypeCounts(entries)
	)
	for _, entry := range entries {
		testSuite.feedBackend.CreateFeedEntry(entry)
	}

	testSuite.RefreshIndices(persistence.IndexNameFeeds)
	defer GetTestSuite().DeleteAllDocuments()

	t.Run("Test to see if 1001 actions were counted",
		func(t *testing.T) {
			res, err := testSuite.feedClient.GetFeedSummary(ctx, &request)
			if assert.Nil(t, err) {
				assert.Equal(t, int64(1001), res.TotalEntries)

				assert.Equal(t, len(expectedCounts.EntryCounts), len(res.EntryCounts),
					"Expected number of Counts does not match results %d != %d", len(expectedCounts.EntryCounts), len(res.EntryCounts))

				for _, count := range expectedCounts.EntryCounts {
					foundCount, found := findCount(res.EntryCounts, count)
					assert.True(t, found, "Count %s was not found in results", count.Category)

					assert.Equal(t, count.Count, foundCount.Count,
						"Expected number of Counts does not match results %s = %s %d != %d",
						count.Category, foundCount.Category, count.Count, foundCount.Count)
				}
			}
		})
}

func TestFeedCountsCountOnlyFilteredUsers(t *testing.T) {
	var (
		totalEntries = 24
		startDate    = time.Now().UTC()
		timeDiff     = int(time.Hour) * -2
		entries      = createEntries(startDate, totalEntries, timeDiff)
	)

	for _, entry := range entries {
		testSuite.feedBackend.CreateFeedEntry(entry)
	}

	testSuite.RefreshIndices(persistence.IndexNameFeeds)
	defer GetTestSuite().DeleteAllDocuments()

	cases := []testCase{
		{
			description: "should count only 'User' events",
			request: event_feed.FeedSummaryRequest{
				CountCategory: "event-type",
				Filters:       []string{"requestor_name:User"},
			},
			expectedCounts: entriesToTypeCounts(filter(entries, func(a *feed.FeedEntry) bool {
				return a.ActorName == "User"
			})),
		},
		{
			description: "should count only 'UI User' actions",
			request: event_feed.FeedSummaryRequest{
				CountCategory: "event-type",
				Filters:       []string{"requestor_name:UI User"},
			},
			expectedCounts: entriesToTypeCounts(filter(entries, func(a *feed.FeedEntry) bool {
				return a.ActorName == "UI User"
			})),
		},
		{
			description: "should count 'User' and 'UI User' actions",
			request: event_feed.FeedSummaryRequest{
				CountCategory: "event-type",
				Filters:       []string{"requestor_name:UI User", "requestor_name:User"},
			},
			expectedCounts: entriesToTypeCounts(filter(entries, func(a *feed.FeedEntry) bool {
				return a.ActorName == "UI User" || a.ActorName == "User"
			})),
		},
	}

	// Run all the cases!
	runCases(t, cases)
}

func TestFeedCountsTypeCountsProjectFilter(t *testing.T) {
	startDate := time.Now().UTC()
	request := &event_feed.FeedSummaryRequest{
		CountCategory: "event-type",
		Start:         startDate.AddDate(0, 0, -11).Unix() * 1000,
	}

	cases := []struct {
		description string
		entries     []feed.FeedEntry
		ctx         context.Context
		expected    *event_feed.FeedSummaryResponse
	}{
		{
			description: "No Events with requesting projects",
			entries:     []feed.FeedEntry{},
			ctx:         contextWithProjects([]string{"project9"}),
			expected:    &event_feed.FeedSummaryResponse{},
		},
		{
			description: "Return the only event; One event with a project matching requested projects",
			entries: []feed.FeedEntry{
				{
					Projects:           []string{"project9"},
					ObjectObjectType:   "cookbook",
					ProducerObjectType: "chef_server",
				},
			},
			ctx: contextWithProjects([]string{"project9"}),
			expected: &event_feed.FeedSummaryResponse{
				TotalEntries: 1,
				EntryCounts: []*event_feed.EntryCount{
					{
						Category: "cookbook",
						Count:    1,
					},
				},
			},
		},
		{
			description: "Return the only event; One non chef-server event with no projects; requesting one project",
			entries: []feed.FeedEntry{
				{
					ObjectObjectType:   "profile",
					ProducerObjectType: "profile",
				},
			},
			ctx: contextWithProjects([]string{"project9"}),
			expected: &event_feed.FeedSummaryResponse{
				TotalEntries: 1,
				EntryCounts: []*event_feed.EntryCount{
					{
						Category: "profile",
						Count:    1,
					},
				},
			},
		},
		{
			description: "Return both events; Two events with a project matching requested projects",
			entries: []feed.FeedEntry{
				{
					ObjectObjectType:   "cookbook",
					Projects:           []string{"project9"},
					ProducerObjectType: "chef_server",
				},
				{
					ObjectObjectType:   "node",
					Projects:           []string{"project9"},
					ProducerObjectType: "chef_server",
				},
			},
			ctx: contextWithProjects([]string{"project9"}),
			expected: &event_feed.FeedSummaryResponse{
				TotalEntries: 2,
				EntryCounts: []*event_feed.EntryCount{
					{
						Category: "cookbook",
						Count:    1,
					},
					{
						Category: "node",
						Count:    1,
					},
				},
			},
		},
		{
			description: "Return both events; Two events one chef-server and the other not; chef-server event has a project matching the requested project",
			entries: []feed.FeedEntry{
				{
					ObjectObjectType:   "cookbook",
					Projects:           []string{"project9"},
					ProducerObjectType: "chef_server",
				},
				{
					ObjectObjectType:   "profile",
					ProducerObjectType: "profile",
				},
			},
			ctx: contextWithProjects([]string{"project9"}),
			expected: &event_feed.FeedSummaryResponse{
				TotalEntries: 2,
				EntryCounts: []*event_feed.EntryCount{
					{
						Category: "cookbook",
						Count:    1,
					},
					{
						Category: "profile",
						Count:    1,
					},
				},
			},
		},
		{
			description: "Return non chef-server event count; Two events one chef-server and the other not; chef-server event has a non-matching project to the requested project",
			entries: []feed.FeedEntry{
				{
					ObjectObjectType:   "cookbook",
					Projects:           []string{"not-matching-project"},
					ProducerObjectType: "chef_server",
				},
				{
					ObjectObjectType:   "profile",
					ProducerObjectType: "profile",
				},
			},
			ctx: contextWithProjects([]string{"project9"}),
			expected: &event_feed.FeedSummaryResponse{
				TotalEntries: 1,
				EntryCounts: []*event_feed.EntryCount{
					{
						Category: "profile",
						Count:    1,
					},
				},
			},
		},
		{
			description: "Return one of the events;Two Events with only one's project matching requested projects",
			entries: []feed.FeedEntry{
				{
					ObjectObjectType:   "cookbook",
					Projects:           []string{"project9"},
					ProducerObjectType: "chef_server",
				},
				{
					ObjectObjectType:   "node",
					Projects:           []string{"project3"},
					ProducerObjectType: "chef_server",
				},
			},
			ctx: contextWithProjects([]string{"project9"}),
			expected: &event_feed.FeedSummaryResponse{
				TotalEntries: 1,
				EntryCounts: []*event_feed.EntryCount{
					{
						Category: "cookbook",
						Count:    1,
					},
				},
			},
		},
		{
			description: "Return not counts; One event with a project not matching the request's projects",
			entries: []feed.FeedEntry{
				{
					ObjectObjectType:   "cookbook",
					Projects:           []string{"project9"},
					ProducerObjectType: "chef_server",
				},
			},
			ctx: contextWithProjects([]string{"project3"}),
			expected: &event_feed.FeedSummaryResponse{
				TotalEntries: 0,
				EntryCounts:  []*event_feed.EntryCount{},
			},
		},
		{
			description: "Return the only event; One event with one project; request is for all projects",
			ctx:         contextWithProjects([]string{authzConstants.AllProjectsExternalID}),
			entries: []feed.FeedEntry{
				{
					ObjectObjectType:   "cookbook",
					Projects:           []string{"project9"},
					ProducerObjectType: "chef_server",
				},
			},
			expected: &event_feed.FeedSummaryResponse{
				TotalEntries: 1,
				EntryCounts: []*event_feed.EntryCount{
					{
						Category: "cookbook",
						Count:    1,
					},
				},
			},
		},
		{
			description: "Return all three events; Three events with different projects and one without any projects; request all projects allowed",
			ctx:         contextWithProjects([]string{authzConstants.AllProjectsExternalID}),
			entries: []feed.FeedEntry{
				{
					ObjectObjectType:   "cookbook",
					Projects:           []string{"project9"},
					ProducerObjectType: "chef_server",
				},
				{
					ObjectObjectType:   "node",
					Projects:           []string{"project12"},
					ProducerObjectType: "chef_server",
				},
				{
					ObjectObjectType:   "bag",
					Projects:           []string{},
					ProducerObjectType: "chef_server",
				},
			},
			expected: &event_feed.FeedSummaryResponse{
				TotalEntries: 3,
				EntryCounts: []*event_feed.EntryCount{
					{
						Category: "cookbook",
						Count:    1,
					},
					{
						Category: "node",
						Count:    1,
					},
					{
						Category: "bag",
						Count:    1,
					},
				},
			},
		},
		{
			description: "Return the only event; One event with no projects; request for all projects",
			ctx:         contextWithProjects([]string{authzConstants.AllProjectsExternalID}),
			entries: []feed.FeedEntry{
				{
					ObjectObjectType:   "cookbook",
					Projects:           []string{},
					ProducerObjectType: "chef_server",
				},
			},
			expected: &event_feed.FeedSummaryResponse{
				TotalEntries: 1,
				EntryCounts: []*event_feed.EntryCount{
					{
						Category: "cookbook",
						Count:    1,
					},
				},
			},
		},
		{
			description: "Return the only event; One event that has no projects; request unassigned projects allowed",
			ctx:         contextWithProjects([]string{authzConstants.UnassignedProjectID}),
			entries: []feed.FeedEntry{
				{
					ObjectObjectType:   "cookbook",
					Projects:           []string{},
					ProducerObjectType: "chef_server",
				},
			},
			expected: &event_feed.FeedSummaryResponse{
				TotalEntries: 1,
				EntryCounts: []*event_feed.EntryCount{
					{
						Category: "cookbook",
						Count:    1,
					},
				},
			},
		},
		{
			description: "Returns zero count; One event with a project; request only unassigned projects",
			ctx:         contextWithProjects([]string{authzConstants.UnassignedProjectID}),
			entries: []feed.FeedEntry{
				{
					ObjectObjectType:   "cookbook",
					Projects:           []string{"project9"},
					ProducerObjectType: "chef_server",
				},
			},
			expected: &event_feed.FeedSummaryResponse{
				TotalEntries: 0,
				EntryCounts:  []*event_feed.EntryCount{},
			},
		},
		{
			description: "Returns one count; Two events have and don't have projects; requesting only unassigned projects",
			ctx:         contextWithProjects([]string{authzConstants.UnassignedProjectID}),
			entries: []feed.FeedEntry{
				{
					ObjectObjectType:   "cookbook",
					Projects:           []string{"project9"},
					ProducerObjectType: "chef_server",
				},
				{
					ObjectObjectType:   "node",
					Projects:           []string{},
					ProducerObjectType: "chef_server",
				},
			},
			expected: &event_feed.FeedSummaryResponse{
				TotalEntries: 1,
				EntryCounts: []*event_feed.EntryCount{
					{
						Category: "node",
						Count:    1,
					},
				},
			},
		},
		{
			description: "Return one count; One event with a project; request unassigned and a matching project allowed",
			ctx:         contextWithProjects([]string{authzConstants.UnassignedProjectID, "project9"}),
			entries: []feed.FeedEntry{
				{
					ObjectObjectType:   "cookbook",
					Projects:           []string{"project9"},
					ProducerObjectType: "chef_server",
				},
			},
			expected: &event_feed.FeedSummaryResponse{
				TotalEntries: 1,
				EntryCounts: []*event_feed.EntryCount{
					{
						Category: "cookbook",
						Count:    1,
					},
				},
			},
		},
		{
			description: "Return one event count; One event with no projects; request has no projects",
			ctx:         contextWithProjects([]string{}),
			entries: []feed.FeedEntry{
				{
					ObjectObjectType:   "cookbook",
					Projects:           []string{},
					ProducerObjectType: "chef_server",
				},
			},
			expected: &event_feed.FeedSummaryResponse{
				TotalEntries: 1,
				EntryCounts: []*event_feed.EntryCount{
					{
						Category: "cookbook",
						Count:    1,
					},
				},
			},
		},

		{
			description: "Return both event counts;Two events have and don't have projects; request has no projects",
			ctx:         contextWithProjects([]string{}),
			entries: []feed.FeedEntry{
				{
					ObjectObjectType:   "cookbook",
					Projects:           []string{"project9"},
					ProducerObjectType: "chef_server",
				},
				{
					ObjectObjectType:   "node",
					Projects:           []string{},
					ProducerObjectType: "chef_server",
				},
			},
			expected: &event_feed.FeedSummaryResponse{
				TotalEntries: 2,
				EntryCounts: []*event_feed.EntryCount{
					{
						Category: "node",
						Count:    1,
					},
					{
						Category: "cookbook",
						Count:    1,
					},
				},
			},
		},
		{
			description: "Return one event count; One event with one project matching one of several requested projects allowed",
			ctx:         contextWithProjects([]string{"project3", "project9", "project7", "project6"}),
			entries: []feed.FeedEntry{
				{
					ObjectObjectType:   "cookbook",
					Projects:           []string{"project9"},
					ProducerObjectType: "chef_server",
				},
			},
			expected: &event_feed.FeedSummaryResponse{
				TotalEntries: 1,
				EntryCounts: []*event_feed.EntryCount{
					{
						Category: "cookbook",
						Count:    1,
					},
				},
			},
		},
		{
			description: "Return a count of both events; Two events with one project matching different projects of several requested",
			ctx:         contextWithProjects([]string{"project3", "project9", "project7", "project6"}),
			entries: []feed.FeedEntry{
				{
					ObjectObjectType:   "cookbook",
					Projects:           []string{"project9"},
					ProducerObjectType: "chef_server",
				},
				{
					ObjectObjectType:   "node",
					Projects:           []string{"project3"},
					ProducerObjectType: "chef_server",
				},
			},
			expected: &event_feed.FeedSummaryResponse{
				TotalEntries: 2,
				EntryCounts: []*event_feed.EntryCount{
					{
						Category: "node",
						Count:    1,
					},
					{
						Category: "cookbook",
						Count:    1,
					},
				},
			},
		},
		{
			description: "Returns zero counts; One event with one project not matching any of several requested projects allowed",
			ctx:         contextWithProjects([]string{"project3", "project4", "project7", "project6"}),
			entries: []feed.FeedEntry{
				{
					ObjectObjectType:   "cookbook",
					Projects:           []string{"project9"},
					ProducerObjectType: "chef_server",
				},
			},
			expected: &event_feed.FeedSummaryResponse{
				TotalEntries: 0,
				EntryCounts:  []*event_feed.EntryCount{},
			},
		},
		{
			description: "Returns zero counts; Two events with neither having projects matching any of several requested projects allowed",
			ctx:         contextWithProjects([]string{"project3", "project4", "project7", "project6"}),
			entries: []feed.FeedEntry{
				{
					ObjectObjectType:   "cookbook",
					Projects:           []string{"project9"},
					ProducerObjectType: "chef_server",
				},
				{
					ObjectObjectType:   "node",
					Projects:           []string{"project10"},
					ProducerObjectType: "chef_server",
				},
			},
			expected: &event_feed.FeedSummaryResponse{
				TotalEntries: 0,
				EntryCounts:  []*event_feed.EntryCount{},
			},
		},

		{
			description: "Returns one count; One event with several projects where only one matches a single requested project",
			ctx:         contextWithProjects([]string{"project3"}),
			entries: []feed.FeedEntry{
				{
					ObjectObjectType:   "cookbook",
					Projects:           []string{"project3", "project4", "project7", "project6"},
					ProducerObjectType: "chef_server",
				},
			},
			expected: &event_feed.FeedSummaryResponse{
				TotalEntries: 1,
				EntryCounts: []*event_feed.EntryCount{
					{
						Category: "cookbook",
						Count:    1,
					},
				},
			},
		},
		{
			description: "Two events with both having several projects where one matches a single requested project",
			ctx:         contextWithProjects([]string{"project3"}),
			entries: []feed.FeedEntry{
				{
					ObjectObjectType:   "cookbook",
					Projects:           []string{"project3", "project4", "project7", "project6"},
					ProducerObjectType: "chef_server",
				},
				{
					ObjectObjectType:   "node",
					Projects:           []string{"project12", "project10", "project11", "project3"},
					ProducerObjectType: "chef_server",
				},
			},
			expected: &event_feed.FeedSummaryResponse{
				TotalEntries: 2,
				EntryCounts: []*event_feed.EntryCount{
					{
						Category: "node",
						Count:    1,
					},
					{
						Category: "cookbook",
						Count:    1,
					},
				},
			},
		},
		{
			description: "Return the count for one; Two events with several projects where only one of the event's project matches a single requested project",
			ctx:         contextWithProjects([]string{"project3"}),
			entries: []feed.FeedEntry{
				{
					ObjectObjectType:   "cookbook",
					Projects:           []string{"project3", "project4", "project7", "project6"},
					ProducerObjectType: "chef_server",
				},
				{
					ObjectObjectType:   "node",
					Projects:           []string{"project12", "project10", "project11", "project13"},
					ProducerObjectType: "chef_server",
				},
			},
			expected: &event_feed.FeedSummaryResponse{
				TotalEntries: 1,
				EntryCounts: []*event_feed.EntryCount{
					{
						Category: "cookbook",
						Count:    1,
					},
				},
			},
		},
		{
			description: "One event with several projects where one matches one of several requested project",
			ctx:         contextWithProjects([]string{"project3", "project10", "project12", "project13"}),
			entries: []feed.FeedEntry{
				{
					ObjectObjectType:   "cookbook",
					Projects:           []string{"project3", "project4", "project7", "project6"},
					ProducerObjectType: "chef_server",
				},
			},
			expected: &event_feed.FeedSummaryResponse{
				TotalEntries: 1,
				EntryCounts: []*event_feed.EntryCount{
					{
						Category: "cookbook",
						Count:    1,
					},
				},
			},
		},
		{
			description: "Returns both events count; Two events with several projects where one matches one of several requested project",
			ctx:         contextWithProjects([]string{"project3", "project10", "project12", "project13"}),
			entries: []feed.FeedEntry{
				{
					ObjectObjectType:   "cookbook",
					Projects:           []string{"project3", "project4", "project7", "project6"},
					ProducerObjectType: "chef_server",
				},
				{
					ObjectObjectType:   "node",
					Projects:           []string{"project13", "project14", "project17", "project16"},
					ProducerObjectType: "chef_server",
				},
			},
			expected: &event_feed.FeedSummaryResponse{
				TotalEntries: 2,
				EntryCounts: []*event_feed.EntryCount{
					{
						Category: "node",
						Count:    1,
					},
					{
						Category: "cookbook",
						Count:    1,
					},
				},
			},
		},
		{
			description: "Returns zero counts; One event with several projects where none matches several requested projects",
			ctx:         contextWithProjects([]string{"project14", "project10", "project12", "project13"}),
			entries: []feed.FeedEntry{
				{
					ObjectObjectType:   "cookbook",
					Projects:           []string{"project3", "project4", "project7", "project6"},
					ProducerObjectType: "chef_server",
				},
			},
			expected: &event_feed.FeedSummaryResponse{},
		},
		{
			description: "Returns one count; One event with several projects where two matches two of several requested project",
			ctx:         contextWithProjects([]string{"project3", "project10", "project12", "project13"}),
			entries: []feed.FeedEntry{
				{
					ObjectObjectType: "cookbook",
					Projects:         []string{"project3", "project10", "project7", "project6"},
				},
			},
			expected: &event_feed.FeedSummaryResponse{
				TotalEntries: 1,
				EntryCounts: []*event_feed.EntryCount{
					{
						Category: "cookbook",
						Count:    1,
					},
				},
			},
		},
	}

	for _, test := range cases {
		t.Run(fmt.Sprintf("Project filter: %s", test.description), func(t *testing.T) {
			for index := range test.entries {
				test.entries[index].ID = newUUID()
				test.entries[index].Published = time.Now()
				test.entries[index].ProducerName = "Fred"
				test.entries[index].ProducerTags = []string{"mycompany", "engineering department", "compliance team"}
				test.entries[index].FeedType = "event"
				test.entries[index].EventType = event.ScanJobUpdatedEventName
				test.entries[index].Tags = []string{"org_1", "compliance", "profile"}
				test.entries[index].ActorID = "urn:mycompany:user:fred"
				test.entries[index].ActorObjectType = "User"
				test.entries[index].ActorName = "Fred"
				test.entries[index].Verb = "update"
				test.entries[index].ObjectID = "urn:chef:compliance:scan-job"
				test.entries[index].ObjectName = "Scan Job"
				test.entries[index].TargetID = "urn:mycompany:environment:production"
				test.entries[index].TargetObjectType = "Environment"
				test.entries[index].TargetName = "Production"
				test.entries[index].Created = time.Now().UTC()

				testSuite.feedBackend.CreateFeedEntry(&test.entries[index])
			}
			testSuite.RefreshIndices(persistence.IndexNameFeeds)

			defer testSuite.DeleteAllDocuments()

			res, err := testSuite.feedClient.GetFeedSummary(test.ctx, request)
			assert.NoError(t, err)

			// test response
			assert.Equal(t, test.expected.TotalEntries, res.TotalEntries)
			t.Logf("counts %v", res.EntryCounts)
			passert.ElementsMatch(t, test.expected.EntryCounts, res.EntryCounts)
		})
	}
}

func TestTaskCountsReturnOnlyEventsWithinDateRange(t *testing.T) {
	var (
		totalEntries = 10
		startDate    = time.Now().UTC()
		timeDiff     = int(time.Hour) * -24
		entries      = createEntries(startDate, totalEntries, timeDiff)
	)

	for _, entry := range entries {
		testSuite.feedBackend.CreateFeedEntry(entry)
	}

	testSuite.RefreshIndices(persistence.IndexNameFeeds)
	defer GetTestSuite().DeleteAllDocuments()

	expectedCounts := entriesToTaskCounts(entries)

	cases := []testCase{
		{
			description:    "should count 10 events (default)",
			request:        event_feed.FeedSummaryRequest{CountCategory: "task"},
			expectedCounts: expectedCounts,
		},
		{
			description: "should count all 10 events",
			request: event_feed.FeedSummaryRequest{
				CountCategory: "task",
				Start:         startDate.AddDate(0, 0, -11).Unix() * 1000,
			},
			expectedCounts: expectedCounts,
		},
		{
			description: "should count only first event",
			request: event_feed.FeedSummaryRequest{
				CountCategory: "task",
				End:           startDate.AddDate(0, 0, -1).Unix() * 1000,
			},
			expectedCounts: entriesToTaskCounts(entries[1:10]),
		},
		{
			description: "should count only one event",
			request: event_feed.FeedSummaryRequest{
				CountCategory: "task",
				Start:         startDate.AddDate(0, 0, -5).Unix() * 1000,
				End:           startDate.AddDate(0, 0, -5).Unix() * 1000,
			},
			expectedCounts: entriesToTaskCounts(entries[5:6]),
		},
	}

	// Run all the cases!
	runCases(t, cases)
}

func TestTaskCountsCountOnlyFilteredUsers(t *testing.T) {
	var (
		totalEntries = 24
		startDate    = time.Now().UTC()
		timeDiff     = int(time.Hour) * -2
		entries      = createEntries(startDate, totalEntries, timeDiff)
	)

	for _, entry := range entries {
		testSuite.feedBackend.CreateFeedEntry(entry)
	}

	testSuite.RefreshIndices(persistence.IndexNameFeeds)
	defer GetTestSuite().DeleteAllDocuments()

	cases := []testCase{
		{
			description: "should count only 'User' events",
			request: event_feed.FeedSummaryRequest{
				CountCategory: "task",
				Filters:       []string{"requestor_name:User"},
			},
			expectedCounts: entriesToTaskCounts(filter(entries, func(a *feed.FeedEntry) bool {
				return stringutils.SliceContains(a.Tags, "User")
			})),
		},
		{
			description: "should count only 'UI User' actions",
			request: event_feed.FeedSummaryRequest{
				CountCategory: "task",
				Filters:       []string{"requestor_name:UI User"},
			},
			expectedCounts: entriesToTaskCounts(filter(entries, func(a *feed.FeedEntry) bool {
				return stringutils.SliceContains(a.Tags, "UI User")
			})),
		},
		{
			description: "should count 'User' and 'UI User' actions",
			request: event_feed.FeedSummaryRequest{
				CountCategory: "task",
				Filters:       []string{"requestor_name:UI User", "requestor_name:User"},
			},
			expectedCounts: entriesToTaskCounts(filter(entries, func(a *feed.FeedEntry) bool {
				return stringutils.SliceContains(a.Tags, "UI User") || stringutils.SliceContains(a.Tags, "User")
			})),
		},
	}

	// Run all the cases!
	runCases(t, cases)
}

func filter(entries []*feed.FeedEntry, f func(*feed.FeedEntry) bool) []*feed.FeedEntry {
	filteredEntries := make([]*feed.FeedEntry, 0)
	for _, entry := range entries {
		if f(entry) {
			filteredEntries = append(filteredEntries, entry)
		}
	}
	return filteredEntries
}

func createEntries(startDate time.Time, amountToCreate int, timeDiff int) []*feed.FeedEntry {
	var (
		entries     = []*feed.FeedEntry{}
		entityTypes = []string{"profile", "scanjobs"}
		verbs       = []string{"create", "update", "delete"}
	)

	for i := 0; i < amountToCreate; i++ {
		var (
			entityType = entityTypes[rand.Int()%len(entityTypes)]
			id         = newUUID()
			time       = startDate.Add(time.Duration(i * timeDiff))
			name       = entityType + strconv.Itoa(i)
			verb       = verbs[rand.Int()%len(verbs)]
			tags       = []string{entityType, verb}
			user       string
			eventType  string
		)

		if entityType == "scanjobs" {
			user = "UI User"

			switch verb {
			case "create":
				eventType = event.ScanJobCreatedEventName
			case "update":
				eventType = event.ScanJobUpdatedEventName
			case "delete":
				eventType = event.ScanJobDeletedEventName
			}

		} else {
			user = "User"

			switch verb {
			case "create":
				eventType = event.ProfileCreatedEventName
			case "delete":
				eventType = event.ProfileDeletedEventName
			}
		}

		tags = append(tags, user, eventType)

		entry := feed.FeedEntry{
			ID:                 id,
			ProducerID:         "producerId",
			ProducerName:       "producerName",
			ProducerObjectType: "producerType",
			ProducerTags:       []string{"producerTags"},
			FeedType:           "event",
			EventType:          eventType,
			Tags:               tags,
			Published:          time.UTC(),
			ActorID:            "actorId",
			ActorName:          user,
			ActorObjectType:    "actorType",
			Verb:               verb,
			ObjectID:           "objectId",
			ObjectName:         name,
			ObjectObjectType:   entityType,
			TargetID:           "targetId",
			TargetName:         "targetName",
			TargetObjectType:   "targetObjectType",
			Created:            time.UTC(),
		}
		entries = append(entries, &entry)
	}
	return entries
}

func entriesToTypeCounts(entries []*feed.FeedEntry) *event_feed.FeedSummaryResponse {
	typeCounts := map[string]int{}

	for _, entry := range entries {
		_, found := typeCounts[entry.ObjectObjectType]
		if found {
			typeCounts[entry.ObjectObjectType]++
		} else {
			typeCounts[entry.ObjectObjectType] = 1
		}
	}

	entryCounts := make([]*event_feed.EntryCount, len(typeCounts))

	index := 0
	for key, value := range typeCounts {
		entryCounts[index] = &event_feed.EntryCount{
			Category: key,
			Count:    int64(value),
		}
		index++
	}

	return &event_feed.FeedSummaryResponse{
		TotalEntries: int64(len(entries)),
		EntryCounts:  entryCounts,
	}
}

func entriesToTaskCounts(entries []*feed.FeedEntry) *event_feed.FeedSummaryResponse {
	taskCounts := map[string]int{}

	for _, entry := range entries {
		_, found := taskCounts[entry.Verb]
		if found {
			taskCounts[entry.Verb]++
		} else {
			taskCounts[entry.Verb] = 1
		}
	}

	entryCounts := make([]*event_feed.EntryCount, len(taskCounts))

	index := 0
	for key, value := range taskCounts {
		entryCounts[index] = &event_feed.EntryCount{
			Category: key,
			Count:    int64(value),
		}
		index++
	}

	return &event_feed.FeedSummaryResponse{
		TotalEntries: int64(len(entries)),
		EntryCounts:  entryCounts,
	}
}

func runCases(t *testing.T, cases []testCase) {
	ctx := context.Background()
	for _, test := range cases {
		t.Run(fmt.Sprintf("with request '%v' it %s", test.request, test.description),
			func(t *testing.T) {
				res, err := testSuite.feedClient.GetFeedSummary(ctx, &test.request)
				if assert.Nil(t, err) {
					assert.Equal(t, test.expectedCounts.TotalEntries, res.TotalEntries)

					assert.Equal(t, len(test.expectedCounts.EntryCounts), len(res.EntryCounts),
						"Expected number of Counts does not match results %d != %d", len(test.expectedCounts.EntryCounts), len(res.EntryCounts))

					for _, count := range test.expectedCounts.EntryCounts {
						foundCount, found := findCount(res.EntryCounts, count)

						assert.True(t, found, "Count %s was not found in results", count.Category)

						if found {
							assert.Equal(t, count.Count, foundCount.Count,
								"Expected number of Counts does not match results %s = %s %d != %d",
								count.Category, foundCount.Category, count.Count, foundCount.Count)
						}
					}
				}
			})
	}
}

func findCount(counts []*event_feed.EntryCount, matchingCount *event_feed.EntryCount) (*event_feed.EntryCount, bool) {
	for _, count := range counts {
		if count.Category == matchingCount.Category {
			return count, true
		}
	}

	return nil, false
}
