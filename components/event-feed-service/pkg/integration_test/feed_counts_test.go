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
	"github.com/chef/automate/components/event-feed-service/pkg/persistence"
	"github.com/chef/automate/components/event-feed-service/pkg/util"
	"github.com/chef/automate/lib/grpc/grpctest"

	"github.com/chef/automate/components/event-service/server"
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

	_, err := testSuite.feedServer.GetFeedSummary(ctx, &event_feed.FeedSummaryRequest{
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
			res, err := testSuite.feedServer.GetFeedSummary(ctx, &request)
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
				Filters:       []string{"requestorName:User"},
			},
			expectedCounts: entriesToTypeCounts(filter(entries, func(a *util.FeedEntry) bool {
				return stringutils.SliceContains(a.Tags, "User")
			})),
		},
		{
			description: "should count only 'UI User' actions",
			request: event_feed.FeedSummaryRequest{
				CountCategory: "event-type",
				Filters:       []string{"requestorName:UI User"},
			},
			expectedCounts: entriesToTypeCounts(filter(entries, func(a *util.FeedEntry) bool {
				return stringutils.SliceContains(a.Tags, "UI User")
			})),
		},
		{
			description: "should count 'User' and 'UI User' actions",
			request: event_feed.FeedSummaryRequest{
				CountCategory: "event-type",
				Filters:       []string{"requestorName:UI User", "requestorName:User"},
			},
			expectedCounts: entriesToTypeCounts(filter(entries, func(a *util.FeedEntry) bool {
				return stringutils.SliceContains(a.Tags, "UI User") || stringutils.SliceContains(a.Tags, "User")
			})),
		},
	}

	// Run all the cases!
	runCases(t, cases)
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
				Filters:       []string{"requestorName:User"},
			},
			expectedCounts: entriesToTaskCounts(filter(entries, func(a *util.FeedEntry) bool {
				return stringutils.SliceContains(a.Tags, "User")
			})),
		},
		{
			description: "should count only 'UI User' actions",
			request: event_feed.FeedSummaryRequest{
				CountCategory: "task",
				Filters:       []string{"requestorName:UI User"},
			},
			expectedCounts: entriesToTaskCounts(filter(entries, func(a *util.FeedEntry) bool {
				return stringutils.SliceContains(a.Tags, "UI User")
			})),
		},
		{
			description: "should count 'User' and 'UI User' actions",
			request: event_feed.FeedSummaryRequest{
				CountCategory: "task",
				Filters:       []string{"requestorName:UI User", "requestorName:User"},
			},
			expectedCounts: entriesToTaskCounts(filter(entries, func(a *util.FeedEntry) bool {
				return stringutils.SliceContains(a.Tags, "UI User") || stringutils.SliceContains(a.Tags, "User")
			})),
		},
	}

	// Run all the cases!
	runCases(t, cases)
}

func filter(entries []*util.FeedEntry, f func(*util.FeedEntry) bool) []*util.FeedEntry {
	filteredEntries := make([]*util.FeedEntry, 0)
	for _, entry := range entries {
		if f(entry) {
			filteredEntries = append(filteredEntries, entry)
		}
	}
	return filteredEntries
}

func createEntries(startDate time.Time, amountToCreate int, timeDiff int) []*util.FeedEntry {
	var (
		entries     = []*util.FeedEntry{}
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
				eventType = server.ScanJobCreated
			case "update":
				eventType = server.ScanJobUpdated
			case "delete":
				eventType = server.ScanJobDeleted
			}

		} else {
			user = "User"

			switch verb {
			case "create":
				eventType = server.ProfileCreated
			case "delete":
				eventType = server.ProfileDeleted
			}
		}

		tags = append(tags, user, eventType)

		entry := util.FeedEntry{
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
			ActorName:          "actorName",
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

func entriesToTypeCounts(entries []*util.FeedEntry) *event_feed.FeedSummaryResponse {
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

func entriesToTaskCounts(entries []*util.FeedEntry) *event_feed.FeedSummaryResponse {
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
				res, err := testSuite.feedServer.GetFeedSummary(ctx, &test.request)
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
