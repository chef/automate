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

	automate_feed "github.com/chef/automate/components/compliance-service/api/automate-feed"
	feedServer "github.com/chef/automate/components/compliance-service/api/automate-feed/server"
	"github.com/chef/automate/components/compliance-service/feed/util"
	server "github.com/chef/automate/components/event-service/server"

	"github.com/sirupsen/logrus"
	"google.golang.org/grpc/codes"

	"github.com/chef/automate/components/compliance-service/ingest/ingestic/mappings"
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
		request     automate_feed.FeedRequest
	}{
		{
			description: "The Start date is after the End date",
			request: automate_feed.FeedRequest{
				End:   date.AddDate(0, 0, -6).Unix() * 1000,
				Start: date.Unix() * 1000,
				Size:  10,
			},
		},
		{
			description: "Before and After parameters should not both be set",
			request: automate_feed.FeedRequest{
				End:    date.Unix() * 1000,
				Start:  date.AddDate(0, 0, -6).Unix() * 1000,
				Size:   10,
				Before: date.AddDate(0, 0, -3).Unix() * 1000,
				After:  date.AddDate(0, 0, -1).Unix() * 1000,
			},
		},
		{
			description: "If the Before param is set the Cursor param needs to be set also",
			request: automate_feed.FeedRequest{
				End:    date.Unix() * 1000,
				Start:  date.AddDate(0, 0, -6).Unix() * 1000,
				Size:   10,
				Before: date.AddDate(0, 0, -3).Unix() * 1000,
			},
		},
		{
			description: "When the After is set without the Cursor, After must be equal to End",
			request: automate_feed.FeedRequest{
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

			_, err := feedService.GetFeed(ctx, &test.request)

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
		entries      = []*util.FeedEntry{}
	)

	for i := 0; i < totalEntries; i++ {

		var (
			published = startDate.AddDate(0, 0, i*-1)
			created   = published
		)

		data := util.FeedEntry{
			ID:                 uuid.Must(uuid.NewV4()).String(),
			ProducerID:         "urn:mycompany:user:violet",
			ProducerName:       "Violet",
			ProducerObjectType: "user",
			ProducerTags:       []string{"mycompany", "engineering department", "compliance team"},
			FeedType:           "event",
			EventType:          server.ScanJobUpdated,
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

	testSuite.RefreshIndices(mappings.IndexNameFeeds)
	defer GetTestSuite().DeleteAllDocuments()

	expectedEntries, err := feedServer.FromInternalFormatToList(entries)
	if err != nil {
		logrus.Warnf("Couldn't set up expected result set... error converting feed entries from internal format to rpc; error: %v", err)
	}

	cases := []struct {
		description string
		request     automate_feed.FeedRequest
		expected    []*automate_feed.FeedEntry
	}{
		{
			description: "should return all 10 events (default)",
			request: automate_feed.FeedRequest{
				Size: pageSize,
			},
			expected: expectedEntries,
		},
		{
			description: "should return all 10 events",
			request: automate_feed.FeedRequest{
				Start: startDate.AddDate(0, 0, -10).Unix() * 1000,
				Size:  pageSize,
			},
			expected: expectedEntries,
		},
		{
			description: "should return first 5 events",
			request: automate_feed.FeedRequest{
				Start: startDate.AddDate(0, 0, -5).Unix() * 1000,
				Size:  pageSize,
			},
			expected: expectedEntries[0:6],
		},
		{
			description: "should return last 5 events",
			request: automate_feed.FeedRequest{
				End:  startDate.AddDate(0, 0, -5).Unix() * 1000,
				Size: pageSize,
			},
			expected: expectedEntries[5:10],
		},
		{
			description: "should return one event",
			request: automate_feed.FeedRequest{
				Start: startDate.AddDate(0, 0, -5).Unix() * 1000,
				End:   startDate.AddDate(0, 0, -5).Unix() * 1000,
				Size:  pageSize,
			},
			expected: expectedEntries[5:6],
		},
		{
			description: "dates after should return zero events",
			request: automate_feed.FeedRequest{
				Start: startDate.AddDate(0, 0, 1).Unix() * 1000,
				End:   startDate.AddDate(0, 0, 5).Unix() * 1000,
				Size:  pageSize,
			},
			expected: []*automate_feed.FeedEntry{},
		},
		{
			description: "dates before should return zero events",
			request: automate_feed.FeedRequest{
				Start: startDate.AddDate(0, 0, -11).Unix() * 1000,
				End:   startDate.AddDate(0, 0, -10).Unix() * 1000,
				Size:  pageSize,
			},
			expected: []*automate_feed.FeedEntry{},
		},
	}

	// Run all the cases!
	for _, test := range cases {
		t.Run(fmt.Sprintf("with request '%v' it %s", test.request, test.description),
			func(t *testing.T) {
				res, err := feedService.GetFeed(ctx, &test.request)
				if assert.Nil(t, err) {
					assert.Equal(t, len(test.expected), len(res.FeedEntries))
					for index, expectedEvent := range test.expected {
						assert.Equal(t, expectedEvent.SourceEventPublished, res.FeedEntries[index].SourceEventPublished)
					}
				}
			})
	}
}

func TestEventFeedFilterTags(t *testing.T) {
	var (
		ctx          = context.Background()
		totalEntries = 15
		pageSize     = int32(totalEntries)
		entries      = []*util.FeedEntry{}
	)

	for i := 0; i < totalEntries; i++ {
		var (
			name    string
			userURN string
			tags    []string
		)

		if i <= 4 {
			name = "Fred"
			userURN = "urn:mycompany:user:fred"
			tags = []string{"org_1", "compliance", "scanjobs"}
		} else if i < 10 {
			name = "Violet"
			userURN = "urn:mycompany:user:violet"
			tags = []string{"org_2", "compliance", "scanjobs"}
		} else {
			name = "Jonesy"
			userURN = "urn:mycompany:user:jonesy"
			tags = []string{"org_2", "compliance", "profiles"}
		}

		data := util.FeedEntry{
			ID:                 uuid.Must(uuid.NewV4()).String(),
			ProducerID:         userURN,
			ProducerName:       name,
			ProducerObjectType: "user",
			ProducerTags:       []string{"mycompany", "engineering department", "compliance team"},
			FeedType:           "event",
			EventType:          server.ScanJobUpdated,
			Tags:               tags,
			Published:          time.Now().UTC(),
			ActorID:            userURN,
			ActorObjectType:    "User",
			ActorName:          name,
			Verb:               "update",
			ObjectID:           "urn:chef:compliance:scan-job",
			ObjectObjectType:   "ScanJob",
			ObjectName:         "Scan Job",
			TargetID:           "urn:mycompany:environment:production",
			TargetObjectType:   "Environment",
			TargetName:         "Production",
			Created:            time.Now().UTC(),
		}

		entries = append(entries, &data)
		// TODO: write bulk load
		testSuite.feedBackend.CreateFeedEntry(&data)
	}

	testSuite.RefreshIndices(mappings.IndexNameFeeds)
	defer testSuite.DeleteAllDocuments()

	expectedEntries, err := feedServer.FromInternalFormatToList(entries)
	if err != nil {
		logrus.Warnf("Couldn't set up expected result set... error converting feed entries from internal format to rpc; error: %v", err)
		expectedEntries = []*automate_feed.FeedEntry{}
	}

	// reverse the list of expected entries to put them in desc order
	for i, j := 0, len(expectedEntries)-1; i < j; i, j = i+1, j-1 {
		expectedEntries[i], expectedEntries[j] = expectedEntries[j], expectedEntries[i]
	}

	cases := []struct {
		description string
		request     automate_feed.FeedRequest
		expected    []*automate_feed.FeedEntry
	}{
		{
			description: "should return all 10 events (default)",
			request: automate_feed.FeedRequest{
				Size: pageSize,
			},
			expected: expectedEntries,
		},
		{
			description: "should return only 'org_2' and profiles events",
			request: automate_feed.FeedRequest{
				Filters: []string{"org_name:org_2", "entity_type:profiles"},
				Size:    pageSize,
			},
			expected: expectedEntries[0:5],
		},
		{
			description: "should return only 'org_1' organization events",
			request: automate_feed.FeedRequest{
				Filters: []string{"org_name:org_1"},
				Size:    pageSize,
			},
			expected: expectedEntries[10:15],
		},
		{
			description: "should return only 'org_2' organization events",
			request: automate_feed.FeedRequest{
				Filters: []string{"org_name:org_2"},
				Size:    pageSize,
			},
			expected: expectedEntries[0:10],
		},
	}

	// Run all the cases!
	for _, test := range cases {
		t.Run(fmt.Sprintf("with request '%v' it %s", test.request, test.description),
			func(t *testing.T) {
				res, err := feedService.GetFeed(ctx, &test.request)
				if assert.Nil(t, err) {
					expectedEvents := &automate_feed.FeedResponse{FeedEntries: test.expected, TotalEntries: int64(len(test.expected))}
					assert.Equal(t, expectedEvents, res)
				}
			})
	}
}

func TestEventFeedFilterEventType(t *testing.T) {
	var (
		ctx          = context.Background()
		totalEntries = 12
		pageSize     = int32(totalEntries)
		entries      = []*util.FeedEntry{}
		eventTypes   = []string{"profiles", "scanjob"}
	)

	for i := 0; i < totalEntries; i++ {
		var (
			name             = "Fred"
			userURN          = "urn:mycompany:user:fred"
			eventType        = server.ScanJobUpdated
			tags             = []string{"org_1", "compliance", eventTypes[1]}
			verb             = "update"
			objectID         = "urn:chef:compliance:scan-job"
			objectObjectType = "ScanJob"
			objectName       = "Scan Job"
		)

		if i > 5 {
			name = "Violet"
			userURN = "urn:mycompany:user:violet"
			eventType = server.ProfileCreated
			tags = []string{"org_2", "compliance", eventTypes[0]}
			verb = "create"
			objectID = "urn:chef:compliance:profile"
			objectObjectType = "Profile"
			objectName = "Profile"
		}

		data := util.FeedEntry{
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

	testSuite.RefreshIndices(mappings.IndexNameFeeds)
	defer testSuite.DeleteAllDocuments()

	expectedEntries, err := feedServer.FromInternalFormatToList(entries)
	if err != nil {
		logrus.Warnf("Couldn't set up expected result set... error converting feed entries from internal format to rpc; error: %v", err)
		expectedEntries = []*automate_feed.FeedEntry{}
	}

	// reverse the list of expected entries to put them in desc order
	for i, j := 0, len(expectedEntries)-1; i < j; i, j = i+1, j-1 {
		expectedEntries[i], expectedEntries[j] = expectedEntries[j], expectedEntries[i]
	}

	cases := []struct {
		description string
		request     automate_feed.FeedRequest
		expected    []*automate_feed.FeedEntry
	}{
		{
			description: "should return all 12 events (default)",
			request: automate_feed.FeedRequest{
				Size: pageSize,
			},
			expected: expectedEntries,
		},
		{
			description: "should return only 6 profile type events",
			request: automate_feed.FeedRequest{
				Filters: []string{"entity_type:" + eventTypes[0]},
				Size:    pageSize,
			},
			expected: expectedEntries[0:6],
		},
		{
			description: "should return only 6 scan job type events",
			request: automate_feed.FeedRequest{
				Filters: []string{"entity_type:" + eventTypes[1]},
				Size:    pageSize,
			},
			expected: expectedEntries[6:12],
		},
	}

	// Run all the cases!
	for _, test := range cases {
		t.Run(fmt.Sprintf("with request '%v' it %s", test.request, test.description),
			func(t *testing.T) {
				res, err := feedService.GetFeed(ctx, &test.request)
				if assert.Nil(t, err) {
					expectedEvents := &automate_feed.FeedResponse{FeedEntries: test.expected, TotalEntries: int64(len(test.expected))}
					assert.Equal(t, expectedEvents, res)
				}
			})
	}
}

/*func TestEventFeedPaginationWalkingFromFirstToLastPages(t *testing.T) {
	var (
		ctx      = context.Background()
		endDate  = time.Now()
		timeDiff = int(time.Hour) * -1
	)
	cases := []struct {
		description  string
		pageSize     int
		totalActions int
	}{
		{
			description:  "Last page full (default)",
			pageSize:     5,
			totalActions: 15,
		},
		{
			description:  "Last page not full",
			pageSize:     5,
			totalActions: 17,
		},
		{
			description:  "Only one full page",
			pageSize:     20,
			totalActions: 20,
		},
		{
			description:  "Only one non-full page",
			pageSize:     20,
			totalActions: 18,
		},
		{
			description:  "5 days of events non even pages",
			pageSize:     9,
			totalActions: 120,
		},
	}
	// Run all the cases!
	for _, test := range cases {
		t.Run(fmt.Sprintf("with request %s", test.description),
			func(t *testing.T) {
				var (
					actions                 = createActions(endDate, test.totalActions, timeDiff)
					expectedEventCollection = actionsToEvents(actions)
				)
				suite.IngestActions(actions)

				resPage, err := cfgmgmt.GetEventFeed(ctx, &automate_feed.FeedRequest{
					Start:    endDate.AddDate(0, 0, -6).Unix() * 1000,
					End:      endDate.Unix() * 1000,
					PageSize: int32(test.pageSize),
				})
				startRange := 0
				endRange := test.pageSize
				if endRange > test.totalActions {
					endRange = test.totalActions
				}
				if assert.Nil(t, err) {
					expectedEvents := &response.Events{
						Events:      expectedEventCollection[startRange:endRange],
						TotalEvents: int64(test.totalActions),
					}
					assert.Equal(t, expectedEvents, resPage)
				}

				numberOfPages := int(math.Ceil(float64(test.totalActions) / float64(test.pageSize)))
				for page := 1; page < numberOfPages; page++ {
					lastPreviousIndex := test.pageSize*page - 1
					resPageLastEvent := resPage.Events[test.pageSize-1]
					expectedLastEvent := expectedEventCollection[lastPreviousIndex]
					assert.Equal(t, expectedLastEvent, resPageLastEvent)

					before := toMilliseconds(expectedLastEvent.Timestamp)
					resPage, err = cfgmgmt.GetEventFeed(ctx, &automate_feed.FeedRequest{
						Start:    endDate.AddDate(0, 0, -6).Unix() * 1000,
						End:      endDate.Unix() * 1000,
						PageSize: int32(test.pageSize),
						Before:   before,
						Cursor:   expectedLastEvent.Id,
					})

					startRange := test.pageSize * page
					endRange := test.pageSize * (page + 1)
					if endRange > test.totalActions {
						endRange = test.totalActions
					}
					if assert.Nil(t, err) {
						expectedEvents := &response.Events{
							Events:      expectedEventCollection[startRange:endRange],
							TotalEvents: int64(test.totalActions),
						}
						assert.Equal(t, expectedEvents, resPage)
					}
				}

				suite.DeleteAllDocuments()
			})
	}
}

// Cursor Dates and Cursor IDs will not always match with the data in this service.
// These tests will ensure that the retrieving of pages will work in these cases.
func TestEventFeedPaginationRandomCursor(t *testing.T) {
	var (
		ctx                          = context.Background()
		endDate                      = time.Now()
		timeDiff                     = int(time.Hour) * -1
		halfHourInMilliseconds int64 = 30 * 60 * 1000
	)
	cases := []struct {
		description  string
		pageSize     int
		totalActions int
	}{
		{
			description:  "Last page full (default)",
			pageSize:     5,
			totalActions: 15,
		},
		{
			description:  "Last page not full",
			pageSize:     5,
			totalActions: 17,
		},
		{
			description:  "Only one full page",
			pageSize:     20,
			totalActions: 20,
		},
		{
			description:  "Only one non-full page",
			pageSize:     20,
			totalActions: 18,
		},
		{
			description:  "5 days of events non even pages",
			pageSize:     9,
			totalActions: 120,
		},
	}
	// Run all the cases!
	for _, test := range cases {
		t.Run(fmt.Sprintf("with request %s", test.description),
			func(t *testing.T) {
				var (
					actions                 = createActions(endDate, test.totalActions, timeDiff)
					expectedEventCollection = actionsToEvents(actions)
				)
				suite.IngestActions(actions)

				// First page, no cursors
				resPage, err := cfgmgmt.GetEventFeed(ctx, &automate_feed.FeedRequest{
					Start:    endDate.AddDate(0, 0, -6).Unix() * 1000,
					End:      endDate.Unix() * 1000,
					PageSize: int32(test.pageSize),
				})
				startRange := 0
				endRange := test.pageSize
				if endRange > test.totalActions {
					endRange = test.totalActions
				}
				if assert.Nil(t, err) {
					expectedEvents := &response.Events{
						Events:      expectedEventCollection[startRange:endRange],
						TotalEvents: int64(test.totalActions),
					}
					assert.Equal(t, expectedEvents, resPage)
				}

				numberOfPages := int(math.Ceil(float64(test.totalActions) / float64(test.pageSize)))
				for page := 1; page < numberOfPages; page++ {
					lastPreviousIndex := test.pageSize*page - 1
					resPageLastEvent := resPage.Events[test.pageSize-1]
					expectedLastEvent := expectedEventCollection[lastPreviousIndex]
					assert.Equal(t, expectedLastEvent, resPageLastEvent)

					before := toMilliseconds(expectedLastEvent.Timestamp)
					// next page. random ID cursor and a date between the actural last event
					// and the next event on the next page.
					resPage, err = cfgmgmt.GetEventFeed(ctx, &automate_feed.FeedRequest{
						Start:    endDate.AddDate(0, 0, -6).Unix() * 1000,
						End:      endDate.Unix() * 1000,
						PageSize: int32(test.pageSize),
						Before:   before - halfHourInMilliseconds,
						Cursor:   "fake",
					})

					startRange := test.pageSize * page
					endRange := test.pageSize * (page + 1)
					if endRange > test.totalActions {
						endRange = test.totalActions
					}
					if assert.Nil(t, err) {
						expectedEvents := &response.Events{
							Events:      expectedEventCollection[startRange:endRange],
							TotalEvents: int64(test.totalActions),
						}
						assert.Equal(t, expectedEvents, resPage)
					}
				}

				suite.DeleteAllDocuments()
			})
	}
}

// Testing when there are two events at the same time on two pages.
// We are ensuring that these duplicate events are not on more than one page.
func TestEventFeedPaginationSameTime(t *testing.T) {
	var (
		ctx      = context.Background()
		endDate  = time.Now()
		timeDiff = int(time.Hour) * -1
	)
	cases := []struct {
		description  string
		pageSize     int
		totalActions int
	}{
		{
			description:  "Last page full (default)",
			pageSize:     5,
			totalActions: 15,
		},
		{
			description:  "Last page not full",
			pageSize:     5,
			totalActions: 17,
		},
		{
			description:  "5 days of events non even pages",
			pageSize:     9,
			totalActions: 120,
		},
	}

	// Run all the cases!
	for _, test := range cases {
		t.Run(fmt.Sprintf("with request %s", test.description),
			func(t *testing.T) {
				var (
					actions         = []iBackend.InternalChefAction{}
					entityTypes     = []string{"cookbook", "bag", "item", "scan job", "node", "client"}
					tasks           = []string{"create", "update", "delete"}
					timesSeenEvents = map[string]int{}
				)

				for i := 0; i < test.totalActions; i++ {
					var (
						entityType = entityTypes[rand.Int()%len(entityTypes)]
						id         = newUUID()
						name       = "action_" + strconv.Itoa(i)
						task       = tasks[rand.Int()%len(tasks)]
						time       = endDate.Add(time.Duration(i * timeDiff))
					)

					// On the new page use the time of the last event of the last page.
					if i > 0 && i%test.pageSize == 0 {
						time = actions[i-1].RecordedAt
					}

					data := iBackend.InternalChefAction{
						Id:         id,
						RecordedAt: time,
						EntityName: name,
						EntityType: entityType,
						Task:       task,
					}
					actions = append(actions, data)
				}

				expectedEventCollection := actionsToEvents(actions)

				for _, expectedEvent := range expectedEventCollection {
					timesSeenEvents[expectedEvent.Id] = 0
				}

				suite.IngestActions(actions)

				resPage, err := cfgmgmt.GetEventFeed(ctx, &automate_feed.FeedRequest{
					Start:    endDate.AddDate(0, 0, -6).Unix() * 1000,
					End:      endDate.Unix() * 1000,
					PageSize: int32(test.pageSize),
				})

				for _, event := range resPage.Events {
					timesSeenEvents[event.Id]++
				}

				endRange := test.pageSize
				if endRange > test.totalActions {
					endRange = test.totalActions
				}
				if assert.Nil(t, err) {
					lastEvent1 := expectedEventCollection[endRange-1]
					lastEvent2 := expectedEventCollection[endRange]
					resLast := resPage.Events[test.pageSize-1]
					assert.True(t, lastEvent1.Id == resLast.Id || lastEvent2.Id == resLast.Id)
				}

				numberOfPages := int(math.Ceil(float64(test.totalActions) / float64(test.pageSize)))
				for page := 1; page < numberOfPages; page++ {
					resPageLastEvent := resPage.Events[test.pageSize-1]

					before := toMilliseconds(resPageLastEvent.Timestamp)
					resPage, err = cfgmgmt.GetEventFeed(ctx, &automate_feed.FeedRequest{
						Start:    endDate.AddDate(0, 0, -6).Unix() * 1000,
						End:      endDate.Unix() * 1000,
						PageSize: int32(test.pageSize),
						Before:   before,
						Cursor:   resPageLastEvent.Id,
					})

					for _, event := range resPage.Events {
						timesSeenEvents[event.Id]++
					}

					if assert.Nil(t, err) {
						endRange := test.pageSize * (page + 1)
						if endRange < test.totalActions {
							// The last event on the response could be two events
							lastEvent1 := expectedEventCollection[endRange-1]
							lastEvent2 := expectedEventCollection[endRange]
							resLast := resPage.Events[test.pageSize-1]
							assert.True(t, lastEvent1.Id == resLast.Id || lastEvent2.Id == resLast.Id)
						}
					}
				}

				for eventId, timesSeen := range timesSeenEvents {
					assert.Equal(t, 1, timesSeen, "event: %s was found %d times", eventId, timesSeen)
				}
				suite.DeleteAllDocuments()
			})
	}
}

func TestEventFeedPaginationWalkingFromLastToFirstPages(t *testing.T) {
	var (
		ctx      = context.Background()
		endDate  = time.Now()
		timeDiff = int(time.Hour) * -1
	)

	cases := []struct {
		description  string
		pageSize     int
		totalActions int
	}{
		{
			description:  "Last to first page (default)",
			pageSize:     5,
			totalActions: 15,
		},
		{
			description:  "Only one full page",
			pageSize:     20,
			totalActions: 20,
		},
		{
			description:  "Only one non-full page",
			pageSize:     20,
			totalActions: 18,
		},
		{
			description:  "Last page not full",
			pageSize:     5,
			totalActions: 17,
		},
		{
			description:  "5 days of events non even pages",
			pageSize:     9,
			totalActions: 120,
		},
	}
	// Run all the cases!
	for _, test := range cases {
		t.Run(fmt.Sprintf("with request %s", test.description),
			func(t *testing.T) {
				var (
					actions                 = createActions(endDate, test.totalActions, timeDiff)
					expectedEventCollection = actionsToEvents(actions)
				)
				suite.IngestActions(actions)

				resPage, err := cfgmgmt.GetEventFeed(ctx, &automate_feed.FeedRequest{
					Start:    endDate.AddDate(0, 0, -6).Unix() * 1000,
					End:      endDate.Unix() * 1000,
					PageSize: int32(test.pageSize),
					After:    endDate.Unix() * 1000,
				})
				startRange := test.totalActions - test.pageSize
				if test.pageSize > test.totalActions {
					startRange = 0
				}
				endRange := test.totalActions
				if assert.Nil(t, err) {
					expectedEvents := &response.Events{
						Events:      expectedEventCollection[startRange:endRange],
						TotalEvents: int64(test.totalActions),
					}
					assert.Equal(t, expectedEvents, resPage)
				}

				numberOfPages := int(math.Ceil(float64(test.totalActions) / float64(test.pageSize)))
				for page := numberOfPages - 1; page > 0; page-- {
					firstPreviousPageIndex := test.totalActions - test.pageSize*(numberOfPages-page)
					resPageFirstEvent := resPage.Events[0]
					expectedFirstEvent := expectedEventCollection[firstPreviousPageIndex]
					assert.Equal(t, expectedFirstEvent, resPageFirstEvent)

					after := toMilliseconds(resPageFirstEvent.Timestamp)

					resPage, err = cfgmgmt.GetEventFeed(ctx, &automate_feed.FeedRequest{
						Start:    endDate.AddDate(0, 0, -6).Unix() * 1000,
						End:      endDate.Unix() * 1000,
						PageSize: int32(test.pageSize),
						After:    after,
						Cursor:   expectedFirstEvent.Id,
					})

					endRange := test.totalActions - test.pageSize*(numberOfPages-page)
					startRange := endRange - test.pageSize
					if startRange < 0 {
						startRange = 0
					}

					if assert.Nil(t, err) {
						expectedEvents := &response.Events{
							Events:      expectedEventCollection[startRange:endRange],
							TotalEvents: int64(test.totalActions),
						}
						assert.Equal(t, expectedEvents, resPage)
					}
				}

				suite.DeleteAllDocuments()
			})
	}
}

func createActions(startDate time.Time, amountToCreate int, timeDiff int) []*util.FeedEntry {
	var (
		entries     		= []*util.FeedEntry{}
		name				= []string{"Fred", "Violet", "Magnus", "Ada"}
		userURNs 			= []string{"urn:mycompany:user:fred", "urn:mycompany:user:violet", "urn:mycompany:user:magnus", "urn:mycompany:user:ada"}
		eventTypes  		= []string{server.ScanJobCreated, server.ScanJobUpdated, server.ScanJobDeleted, server.ProfileCreated, server.ProfileUpdated, server.ProfileDeleted}
		orgs 				= []string{"org1", "org2", "org3"}
		verb       			= []string{"create", "update", "delete"}
		objectID			= []string{"urn:chef:compliance:scan-job", "urn:chef:compliance:profile"}
		objectObjectType	= []string{"ScanJob", "Profile"}
		objectName			= []string{"Scan Job", "Profile"}
	)

	for i := 0; i < amountToCreate; i++ {
		var (
			entityType = entityTypes[rand.Int()%len(entityTypes)]
			id         = newUUID()
			time       = startDate.Add(time.Duration(i * timeDiff))
			name       = "action_" + strconv.Itoa(i)
			org        = orgs[rand.Int()%len(orgs)]
			chefServer = chefServers[rand.Int()%len(chefServers)]
			task       = tasks[rand.Int()%len(tasks)]
		)

		data := iBackend.InternalChefAction{
			Id:               id,
			RecordedAt:       time,
			EntityName:       name,
			EntityType:       entityType,
			OrganizationName: org,
			ServiceHostname:  chefServer,
			Task:             task,
		}
		actions = append(actions, data)
	}

	return actions
}*/

func toMilliseconds(timestamp *google_protobuf.Timestamp) int64 {
	return timestamp.GetSeconds()*millisPerSecond +
		int64(timestamp.GetNanos()/nanosPerMillisecond)
}
