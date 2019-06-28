package integration_test

import (
	"context"
	"fmt"
	"math"
	"testing"
	"time"

	"github.com/stretchr/testify/assert"
	"google.golang.org/grpc/codes"

	"math/rand"
	"strconv"

	"github.com/chef/automate/api/interservice/event_feed"
	"github.com/chef/automate/components/event-feed-service/pkg/persistence"
	"github.com/chef/automate/components/event-feed-service/pkg/util"

	event_server "github.com/chef/automate/components/event-service/server"
	"github.com/chef/automate/lib/grpc/grpctest"
)

var now = time.Date(2018, time.February, 2, 12, 34, 56, 0, time.UTC)

func TestEventStringsNormalRequest(t *testing.T) {
	var (
		ctx             = context.Background()
		numberOfBuckets = 7 * 8
		totalActions    = numberOfBuckets - 8
		timezone        = "America/Los_Angeles"
		loc, _          = time.LoadLocation(timezone)
		startDate       = now.In(loc)
		timeDiff        = int(time.Hour) * -3
		entries         = createEntries(startDate, totalActions, timeDiff)
	)

	for _, entry := range entries {
		testSuite.feedBackend.CreateFeedEntry(entry)
	}

	testSuite.RefreshIndices(persistence.IndexNameFeeds)
	defer GetTestSuite().DeleteAllDocuments()

	request := &event_feed.FeedTimelineRequest{
		Interval: 3,
		Start:    startDate.AddDate(0, 0, -6).Format("2006-01-02"),
		End:      startDate.Format("2006-01-02"),
		Timezone: timezone,
	}

	feedTimeline, err := testSuite.feedServer.GetFeedTimeline(ctx, request)

	assert.Nil(t, err)

	printFeedTimeline(feedTimeline, request)

	assert.Equal(t, 3, len(feedTimeline.ActionLines))

	totalItems := 0
	for _, line := range feedTimeline.ActionLines {
		assert.Equal(t, numberOfBuckets, len(line.Slots))
		totalItems = totalItems + countItems(line)
	}

	assert.Equal(t, totalActions, totalItems)
	assert.Equal(t, request.Start, feedTimeline.Start)
	assert.Equal(t, request.End, feedTimeline.End)
}

func TestEventStringsFilterEventType(t *testing.T) {
	var (
		ctx          = context.Background()
		totalEntries = 24
		timezone     = "America/Los_Angeles"
		startDate    = time.Now().AddDate(0, 0, -3)
		endDate      = time.Now()
		verb         = "create"
		entries      = []*util.FeedEntry{}
		entityTypes  = []string{"profile", "scanjobs"}
		users        = []string{"jonesy", "admin", "evie", "maddie", "tommy", "georgie"}
	)

	for i := 0; i < totalEntries; i++ {
		var (
			entityType = entityTypes[i%len(entityTypes)]
			id         = newUUID()
			time       = endDate.Add(time.Duration(i * int(time.Hour) * -2))
			name       = entityType + strconv.Itoa(i)
			verb       = verb
			tags       = []string{entityType, verb}
			user       = users[rand.Int()%len(users)]
			eventType  string
		)

		if entityType == "scanjobs" {
			eventType = event_server.ScanJobCreated
		} else {
			eventType = event_server.ProfileCreated
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
			Published:          time,
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
			Created:            time,
		}
		entries = append(entries, &entry)
	}

	for _, entry := range entries {
		testSuite.feedBackend.CreateFeedEntry(entry)
	}

	testSuite.RefreshIndices(persistence.IndexNameFeeds)
	defer GetTestSuite().DeleteAllDocuments()

	cases := []struct {
		description string
		request     event_feed.FeedTimelineRequest
		expected    map[string]int
	}{
		{
			description: "should contain all 24 items on create string",
			request: event_feed.FeedTimelineRequest{
				Interval: 1,
				Start:    startDate.Format("2006-01-02"),
				End:      endDate.Format("2006-01-02"),
				Timezone: timezone,
			},
			expected: map[string]int{
				"profile":  12,
				"scanjobs": 12,
			},
		},
		{
			description: "should return only the 12 profiles items on the string",
			request: event_feed.FeedTimelineRequest{
				Filters:  []string{"entity_type:profile"},
				Interval: 1,
				Start:    startDate.Format("2006-01-02"),
				End:      endDate.Format("2006-01-02"),
				Timezone: timezone,
			},
			expected: map[string]int{
				"profile": 12,
			},
		},
		{
			description: "should return only the 12 scanjob items on the string",
			request: event_feed.FeedTimelineRequest{
				Filters:  []string{"entity_type:scanjobs"},
				Interval: 1,
				Start:    startDate.Format("2006-01-02"),
				End:      endDate.Format("2006-01-02"),
				Timezone: timezone,
			},
			expected: map[string]int{
				"scanjobs": 12,
			},
		},
		{
			description: "should return no items on the string",
			request: event_feed.FeedTimelineRequest{
				Filters:  []string{"entity_type:fake"},
				Interval: 1,
				Start:    startDate.Format("2006-01-02"),
				End:      endDate.Format("2006-01-02"),
				Timezone: timezone,
			},
			expected: map[string]int{},
		},
	}

	// Run all the cases!
	for _, test := range cases {
		t.Run(fmt.Sprintf("with request '%v' it %s", test.request, test.description),
			func(t *testing.T) {
				res, err := testSuite.feedServer.GetFeedTimeline(ctx, &test.request)
				if assert.Nil(t, err) {
					for _, line := range res.ActionLines {
						if line.Action == verb {

							itemsCount := map[string]int{}
							for _, item := range line.Slots {
								if len(item.Counts) > 0 {
									itemsCount[item.Counts[0].Category]++
								}
							}

							for key, expectedValue := range test.expected {
								value, ok := itemsCount[key]

								assert.True(t, ok, "Entity type '%s' was not found on string", key)
								assert.Equal(t, expectedValue, value,
									"Number of '%s' on string was %v should be %v", key, value, expectedValue)
							}

							for key := range itemsCount {
								if key != "" {
									_, ok := test.expected[key]
									assert.True(t, ok, "Entity type '%s' should not be on the string", key)
								}
							}
						}
					}
				}
			})
	}
}

func TestEventStringsDayZoneActionsRequest(t *testing.T) {
	var (
		ctx             = context.Background()
		numberOfBuckets = 24
		timezone        = "America/Los_Angeles"
		loc, _          = time.LoadLocation(timezone)
		startDate       = now.In(loc)
		totalActions    = 0
	)

	request := &event_feed.FeedTimelineRequest{
		Interval: 1,
		Start:    startDate.Format("2006-01-02"),
		End:      startDate.Format("2006-01-02"),
		Timezone: timezone,
	}

	feedTimeline, err := testSuite.feedServer.GetFeedTimeline(ctx, request)

	assert.Nil(t, err)

	totalItems := 0
	for _, line := range feedTimeline.ActionLines {
		assert.Equal(t, numberOfBuckets, len(line.Slots))
		totalItems = totalItems + countItems(line)
	}

	assert.Equal(t, totalActions, totalItems)
	assert.Equal(t, request.Start, feedTimeline.Start)
	assert.Equal(t, request.End, feedTimeline.End)

	printFeedTimeline(feedTimeline, request)
}

func TestEventStringsIncorrectHoursBetween(t *testing.T) {
	var ctx = context.Background()
	cases := []int{-1, 0, 5, 7, 9, 10, 11, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 25, 27, 98}

	for _, bucketSize := range cases {
		t.Run(fmt.Sprintf("with parameters hours between=%d it should return an error",
			bucketSize), func(t *testing.T) {

			_, err := testSuite.feedServer.GetFeedTimeline(ctx, &event_feed.FeedTimelineRequest{
				Start:    "2018-01-01",
				End:      "2018-01-06",
				Interval: int32(bucketSize),
				Timezone: "UTC",
			})

			grpctest.AssertCode(t, codes.InvalidArgument, err)
		})
	}
}

func TestEventStringsVaryingBucketsSizeRequest(t *testing.T) {
	var (
		ctx          = context.Background()
		timezone     = "America/Los_Angeles"
		loc, _       = time.LoadLocation(timezone)
		endDate      = now.In(loc)
		totalEntries = 3
		timeDiff     = int(time.Hour) * -24
	)

	endDate = time.Date(endDate.Year(), endDate.Month(), endDate.Day(),
		12, 30, 0, 0, endDate.Location())

	entries := createEntries(endDate, totalEntries, timeDiff)

	for _, entry := range entries {
		testSuite.feedBackend.CreateFeedEntry(entry)
	}

	testSuite.RefreshIndices(persistence.IndexNameFeeds)
	defer GetTestSuite().DeleteAllDocuments()

	for _, bucketSize := range []int{1, 2, 3, 4, 6, 8, 12, 24} {
		numberOfBuckets := int(math.Ceil(24.0 * 3.0 / float64(bucketSize)))

		request := &event_feed.FeedTimelineRequest{
			Interval: int32(bucketSize),
			Start:    endDate.AddDate(0, 0, -2).Format("2006-01-02"),
			End:      endDate.Format("2006-01-02"),
			Timezone: timezone,
		}

		feedTimeline, err := testSuite.feedServer.GetFeedTimeline(ctx, request)

		assert.Nil(t, err)
		totalItems := 0
		for _, line := range feedTimeline.ActionLines {
			assert.Equal(t, numberOfBuckets, len(line.Slots), "with bucket size: %d", bucketSize)
			totalItems = totalItems + countItems(line)
		}

		assert.Equal(t, totalEntries, totalItems)
		assert.Equal(t, request.Start, feedTimeline.Start)
		assert.Equal(t, request.End, feedTimeline.End)

		printFeedTimeline(feedTimeline, request)
	}
}

func TestEventStringsVaryingBucketsSizeNoActionsRequest(t *testing.T) {
	var (
		ctx          = context.Background()
		timezone     = "America/Los_Angeles"
		loc, _       = time.LoadLocation(timezone)
		endDate      = now.In(loc)
		totalEntries = 0
	)

	endDate = time.Date(endDate.Year(), endDate.Month(), endDate.Day(),
		12, 30, 0, 0, endDate.Location())

	for _, bucketSize := range []int{1, 2, 3, 4, 6, 8, 12, 24} {
		numberOfBuckets := int(math.Ceil(24.0 * 3.0 / float64(bucketSize)))

		request := &event_feed.FeedTimelineRequest{
			Interval: int32(bucketSize),
			Start:    endDate.AddDate(0, 0, -2).Format("2006-01-02"),
			End:      endDate.Format("2006-01-02"),
			Timezone: timezone,
		}

		feedTimeline, err := testSuite.feedServer.GetFeedTimeline(ctx, request)

		assert.Nil(t, err)
		totalItems := 0
		for _, line := range feedTimeline.ActionLines {
			assert.Equal(t, numberOfBuckets, len(line.Slots), "with bucket size: %d", bucketSize)
			totalItems = totalItems + countItems(line)
		}

		assert.Equal(t, totalEntries, totalItems)
		assert.Equal(t, request.Start, feedTimeline.Start)
		assert.Equal(t, request.End, feedTimeline.End)

		printFeedTimeline(feedTimeline, request)
	}
}

func TestEventStringsThreeDayThreeActionsRequest(t *testing.T) {
	var (
		ctx             = context.Background()
		numberOfBuckets = 24 * 3
		timezone        = "America/Los_Angeles"
		loc, _          = time.LoadLocation(timezone)
		endDate         = now.In(loc)
		totalEntries    = 3
	)
	endDate = time.Date(endDate.Year(), endDate.Month(), endDate.Day(),
		12, 30, 0, 0, endDate.Location())

	entries := []*util.FeedEntry{
		{
			ID:                 newUUID(),
			ProducerID:         "producerId",
			ProducerName:       "producerName",
			ProducerObjectType: "producerType",
			ProducerTags:       []string{"producerTags"},
			FeedType:           "event",
			EventType:          event_server.ProfileDeleted,
			Tags:               []string{"profile", "delete"},
			Published:          endDate,
			ActorID:            "actorId",
			ActorName:          "actorName",
			ActorObjectType:    "actorType",
			Verb:               "delete",
			ObjectID:           "objectId",
			ObjectName:         "entity_1",
			ObjectObjectType:   "profile",
			TargetID:           "targetId",
			TargetName:         "targetName",
			TargetObjectType:   "targetObjectType",
			Created:            endDate,
		},
		{
			ID:                 newUUID(),
			ProducerID:         "producerId",
			ProducerName:       "producerName",
			ProducerObjectType: "producerType",
			ProducerTags:       []string{"producerTags"},
			FeedType:           "event",
			EventType:          event_server.ScanJobUpdated,
			Tags:               []string{"scanjobs", "update"},
			Published:          endDate.AddDate(0, 0, -1),
			ActorID:            "actorId",
			ActorName:          "actorName",
			ActorObjectType:    "actorType",
			Verb:               "update",
			ObjectID:           "objectId",
			ObjectName:         "entity_2",
			ObjectObjectType:   "scanjobs",
			TargetID:           "targetId",
			TargetName:         "targetName",
			TargetObjectType:   "targetObjectType",
			Created:            endDate.AddDate(0, 0, -1),
		},
		{
			ID:                 newUUID(),
			ProducerID:         "producerId",
			ProducerName:       "producerName",
			ProducerObjectType: "producerType",
			ProducerTags:       []string{"producerTags"},
			FeedType:           "event",
			EventType:          event_server.ProfileCreated,
			Tags:               []string{"profile", "create"},
			Published:          endDate.AddDate(0, 0, -2),
			ActorID:            "actorId",
			ActorName:          "actorName",
			ActorObjectType:    "actorType",
			Verb:               "create",
			ObjectID:           "objectId",
			ObjectName:         "entity_3",
			ObjectObjectType:   "profile",
			TargetID:           "targetId",
			TargetName:         "targetName",
			TargetObjectType:   "targetObjectType",
			Created:            endDate.AddDate(0, 0, -2),
		},
	}

	for _, entry := range entries {
		testSuite.feedBackend.CreateFeedEntry(entry)
	}

	testSuite.RefreshIndices(persistence.IndexNameFeeds)
	defer GetTestSuite().DeleteAllDocuments()

	request := &event_feed.FeedTimelineRequest{
		Interval: 1,
		Start:    endDate.AddDate(0, 0, -2).Format("2006-01-02"),
		End:      endDate.Format("2006-01-02"),
		Timezone: timezone,
	}

	feedTimeline, err := testSuite.feedServer.GetFeedTimeline(ctx, request)

	assert.Nil(t, err)

	totalItems := 0
	for _, line := range feedTimeline.ActionLines {
		assert.Equal(t, numberOfBuckets, len(line.Slots))
		totalItems = totalItems + countItems(line)
		if line.Action == "create" {
			for index, item := range line.Slots {
				if index == 12 {
					assert.Equal(t, 1, len(item.Counts))
					assert.Equal(t, "profile", item.Counts[0].Category)
				} else {
					assert.Equal(t, 0, len(item.Counts))
				}
			}
		}
		if line.Action == "update" {
			for index, item := range line.Slots {
				if index == 36 {
					assert.Equal(t, 1, len(item.Counts))
					assert.Equal(t, "scanjobs", item.Counts[0].Category)
				} else {
					assert.Equal(t, 0, len(item.Counts))
				}
			}
		}
		if line.Action == "delete" {
			for index, item := range line.Slots {
				if index == 60 {
					assert.Equal(t, 1, len(item.Counts))
					assert.Equal(t, "profile", item.Counts[0].Category)
				} else {
					assert.Equal(t, 0, len(item.Counts))
				}
			}
		}
	}

	assert.Equal(t, totalEntries, totalItems)
	assert.Equal(t, request.Start, feedTimeline.Start)
	assert.Equal(t, request.End, feedTimeline.End)

	printFeedTimeline(feedTimeline, request)
}

func TestEventStringsThreeDayThreeActionsInNewYorkRequest(t *testing.T) {
	var (
		ctx             = context.Background()
		numberOfBuckets = 24 * 3
		timezone        = "America/New_York"
		loc, _          = time.LoadLocation("UTC")
		endDate         = now.In(loc)
		totalEntries    = 3
	)
	endDate = time.Date(endDate.Year(), endDate.Month(), endDate.Day(),
		17, 30, 0, 0, endDate.Location())

	entries := []*util.FeedEntry{
		{
			ID:                 newUUID(),
			ProducerID:         "producerId",
			ProducerName:       "producerName",
			ProducerObjectType: "producerType",
			ProducerTags:       []string{"producerTags"},
			FeedType:           "event",
			EventType:          event_server.ProfileDeleted,
			Tags:               []string{"profile", "delete"},
			Published:          endDate,
			ActorID:            "actorId",
			ActorName:          "actorName",
			ActorObjectType:    "actorType",
			Verb:               "delete",
			ObjectID:           "objectId",
			ObjectName:         "entity_1",
			ObjectObjectType:   "profile",
			TargetID:           "targetId",
			TargetName:         "targetName",
			TargetObjectType:   "targetObjectType",
			Created:            endDate,
		},
		{
			ID:                 newUUID(),
			ProducerID:         "producerId",
			ProducerName:       "producerName",
			ProducerObjectType: "producerType",
			ProducerTags:       []string{"producerTags"},
			FeedType:           "event",
			EventType:          event_server.ScanJobUpdated,
			Tags:               []string{"scanjobs", "update"},
			Published:          endDate.AddDate(0, 0, -1),
			ActorID:            "actorId",
			ActorName:          "actorName",
			ActorObjectType:    "actorType",
			Verb:               "update",
			ObjectID:           "objectId",
			ObjectName:         "entity_2",
			ObjectObjectType:   "scanjobs",
			TargetID:           "targetId",
			TargetName:         "targetName",
			TargetObjectType:   "targetObjectType",
			Created:            endDate.AddDate(0, 0, -1),
		},
		{
			ID:                 newUUID(),
			ProducerID:         "producerId",
			ProducerName:       "producerName",
			ProducerObjectType: "producerType",
			ProducerTags:       []string{"producerTags"},
			FeedType:           "event",
			EventType:          event_server.ProfileCreated,
			Tags:               []string{"profile", "create"},
			Published:          endDate.AddDate(0, 0, -2),
			ActorID:            "actorId",
			ActorName:          "actorName",
			ActorObjectType:    "actorType",
			Verb:               "create",
			ObjectID:           "objectId",
			ObjectName:         "entity_3",
			ObjectObjectType:   "profile",
			TargetID:           "targetId",
			TargetName:         "targetName",
			TargetObjectType:   "targetObjectType",
			Created:            endDate.AddDate(0, 0, -2),
		},
	}

	for _, entry := range entries {
		testSuite.feedBackend.CreateFeedEntry(entry)
	}

	testSuite.RefreshIndices(persistence.IndexNameFeeds)
	defer GetTestSuite().DeleteAllDocuments()

	request := &event_feed.FeedTimelineRequest{
		Interval: 1,
		Start:    endDate.AddDate(0, 0, -2).Format("2006-01-02"),
		End:      endDate.Format("2006-01-02"),
		Timezone: timezone,
	}

	feedTimeline, err := testSuite.feedServer.GetFeedTimeline(ctx, request)

	assert.Nil(t, err)

	totalItems := 0
	for _, line := range feedTimeline.ActionLines {
		assert.Equal(t, numberOfBuckets, len(line.Slots))
		totalItems = totalItems + countItems(line)
		if line.Action == "create" {
			for index, item := range line.Slots {
				if index == 12 {
					assert.Equal(t, 1, len(item.Counts))
					assert.Equal(t, "profile", item.Counts[0].Category)
				} else {
					assert.Equal(t, 0, len(item.Counts))
				}
			}
		}
		if line.Action == "update" {
			for index, item := range line.Slots {
				if index == 36 {
					assert.Equal(t, 1, len(item.Counts))
					assert.Equal(t, "scanjobs", item.Counts[0].Category)
				} else {
					assert.Equal(t, 0, len(item.Counts))
				}
			}
		}
		if line.Action == "delete" {
			for index, item := range line.Slots {
				if index == 60 {
					assert.Equal(t, 1, len(item.Counts))
					assert.Equal(t, "profile", item.Counts[0].Category)
				} else {
					assert.Equal(t, 0, len(item.Counts))
				}
			}
		}
	}

	assert.Equal(t, totalEntries, totalItems)
	assert.Equal(t, request.Start, feedTimeline.Start)
	assert.Equal(t, request.End, feedTimeline.End)

	printFeedTimeline(feedTimeline, request)
}

func TestEventStringsDayOneActionsOutsideOfRequest(t *testing.T) {
	var (
		ctx                 = context.Background()
		numberOfBuckets     = 24
		timezone            = "America/Los_Angeles"
		loc, _              = time.LoadLocation(timezone)
		startDate           = now.In(loc)
		totalInRangeEntries = 0
		timeDiff            = int(time.Hour) * -1
	)
	startDate = time.Date(startDate.Year(), startDate.Month(), startDate.Day(),
		12, 0, 0, 0, startDate.Location())

	entries := createEntries(startDate, 1, timeDiff)

	for _, entry := range entries {
		testSuite.feedBackend.CreateFeedEntry(entry)
	}

	testSuite.RefreshIndices(persistence.IndexNameFeeds)
	defer GetTestSuite().DeleteAllDocuments()

	request := &event_feed.FeedTimelineRequest{
		Interval: 1,
		Start:    startDate.AddDate(0, 0, -1).Format("2006-01-02"),
		End:      startDate.AddDate(0, 0, -1).Format("2006-01-02"),
		Timezone: timezone,
	}

	feedTimeline, err := testSuite.feedServer.GetFeedTimeline(ctx, request)

	assert.Nil(t, err)

	totalItems := 0
	for _, line := range feedTimeline.ActionLines {
		assert.Equal(t, numberOfBuckets, len(line.Slots))
		totalItems = totalItems + countItems(line)
	}

	assert.Equal(t, totalInRangeEntries, totalItems)
	assert.Equal(t, request.Start, feedTimeline.Start)
	assert.Equal(t, request.End, feedTimeline.End)

	printFeedTimeline(feedTimeline, request)
}

func TestEventStringsDayRequest(t *testing.T) {
	var (
		ctx             = context.Background()
		numberOfBuckets = 24
		timezone        = "America/Los_Angeles"
		loc, _          = time.LoadLocation(timezone)
		startDate       = now.In(loc)
		totalEntries    = 24
		timeDiff        = int(time.Hour) * -1
	)
	startDate = time.Date(startDate.Year(), startDate.Month(), startDate.Day(),
		23, 59, 59, 0, startDate.Location())

	entries := createEntries(startDate, totalEntries, timeDiff)

	for _, entry := range entries {
		testSuite.feedBackend.CreateFeedEntry(entry)
	}

	testSuite.RefreshIndices(persistence.IndexNameFeeds)
	defer GetTestSuite().DeleteAllDocuments()

	request := &event_feed.FeedTimelineRequest{
		Interval: 1,
		Start:    startDate.Format("2006-01-02"),
		End:      startDate.Format("2006-01-02"),
		Timezone: timezone,
	}

	feedTimeline, err := testSuite.feedServer.GetFeedTimeline(ctx, request)

	assert.Nil(t, err)

	totalItems := 0
	for _, line := range feedTimeline.ActionLines {
		assert.Equal(t, numberOfBuckets, len(line.Slots))
		totalItems = totalItems + countItems(line)
	}

	assert.Equal(t, totalEntries, totalItems)
	assert.Equal(t, request.Start, feedTimeline.Start)
	assert.Equal(t, request.End, feedTimeline.End)

	printFeedTimeline(feedTimeline, request)
}

func TestEventStringsIncorrectTimezone(t *testing.T) {
	var ctx = context.Background()
	cases := []event_feed.FeedTimelineRequest{
		{
			Start:    "2018-01-01",
			End:      "2018-01-06",
			Interval: 3,
			Timezone: "",
		},
		{
			Start:    "2018-01-01",
			End:      "2018-01-06",
			Interval: 3,
			Timezone: "fake",
		},
	}

	for _, test := range cases {
		t.Run(fmt.Sprintf("with parameters timezone=%s it should return an error",
			test.Timezone), func(t *testing.T) {

			_, err := testSuite.feedServer.GetFeedTimeline(ctx, &test)

			grpctest.AssertCode(t, codes.InvalidArgument, err)
		})
	}
}

func TestEventStringsIncorrectTimes(t *testing.T) {
	var ctx = context.Background()
	cases := []event_feed.FeedTimelineRequest{
		{
			Start:    "2018-01-06",
			End:      "2018-01-01",
			Interval: 3,
			Timezone: "UTC",
		},
		{
			Start:    "2000-00-00",
			End:      "2000-00-06",
			Interval: 3,
			Timezone: "UTC",
		},
		{
			Start:    "00-00-00",
			End:      "00-00-06",
			Interval: 3,
			Timezone: "UTC",
		},
		{
			Start:    "18-10-10",
			End:      "18-10-16",
			Interval: 3,
			Timezone: "UTC",
		},
		{
			Start:    "20-01-01",
			End:      "20-01-06",
			Interval: 3,
			Timezone: "UTC",
		},
		{
			Start:    "17:01:01",
			End:      "17:01:06",
			Interval: 3,
			Timezone: "UTC",
		},
		{
			Start:    "01-01-2000",
			End:      "06-01-2000",
			Interval: 3,
			Timezone: "UTC",
		},
		{
			Start:    "3000-12",
			End:      "3006-12",
			Interval: 3,
			Timezone: "UTC",
		},

		{
			Start:    "2019",
			End:      "2018",
			Interval: 3,
			Timezone: "UTC",
		},
		{
			Start:    "1888:01:01",
			End:      "1888:01:06",
			Interval: 3,
			Timezone: "UTC",
		},
		{
			Start:    "2027/01/01",
			End:      "2027/01/08",
			Interval: 3,
			Timezone: "UTC",
		},
	}

	for _, test := range cases {
		t.Run(fmt.Sprintf("with parameters start=%s end=%s it should return an error",
			test.Start, test.End), func(t *testing.T) {

			_, err := testSuite.feedServer.GetFeedTimeline(ctx, &test)

			grpctest.AssertCode(t, codes.InvalidArgument, err)
		})
	}
}

func countItems(line *event_feed.ActionLine) int {
	totalItems := 0
	for _, item := range line.Slots {
		if len(item.Counts) > 0 {
			if item.Counts[0].Category != "" {
				totalItems++
			}
		}
	}
	return totalItems
}

func printFeedTimeline(timeline *event_feed.FeedTimelineResponse, request *event_feed.FeedTimelineRequest) {

	counts := make([]int, len(timeline.ActionLines))

	numberOfBucketsPerDay := int(math.Ceil(24.0 / float64(timeline.Interval)))
	for index, line := range timeline.ActionLines {
		counts[index] = countItems(line)
	}

	if len(timeline.ActionLines) <= 0 {
		return
	}

	length := len(timeline.ActionLines[0].Slots)

	for index, line := range timeline.ActionLines {
		fmt.Printf("%s = %d ", line.Action, counts[index])
	}
	fmt.Println()

	loc, err := time.LoadLocation(request.Timezone)
	if err != nil {
		return
	}

	startTime, err := time.ParseInLocation("2006-01-02", timeline.Start, loc)
	startTime = time.Date(startTime.Year(), startTime.Month(), startTime.Day(),
		0, 0, 0, 0, startTime.Location())
	endTime, err := time.ParseInLocation("2006-01-02", request.End, loc)
	endTime = time.Date(endTime.Year(), endTime.Month(), endTime.Day(),
		23, 59, 59, 0, endTime.Location())

	dates := make([]string, 0)
	for startTime.Before(endTime) {
		dates = append(dates, startTime.Format("Jan _2"))
		startTime = startTime.AddDate(0, 0, 1)
	}

	for index := 0; index < length; index++ {
		fmt.Print("---")
	}
	for index := 0; index <= len(dates); index++ {
		fmt.Print("-")
	}
	fmt.Println()

	fmt.Print("|")
	numberOfBufferSpaces := (numberOfBucketsPerDay*3 - 6) / 2
	for _, date := range dates {
		for i := 0; i < numberOfBufferSpaces; i++ {
			fmt.Print(" ")
		}
		fmt.Printf("%s", date)
		for i := 0; i < numberOfBufferSpaces; i++ {
			fmt.Print(" ")
		}
		fmt.Print("|")
	}

	fmt.Println()

	for _, line := range timeline.ActionLines {
		for index, eventType := range line.Slots {
			if index%numberOfBucketsPerDay == 0 {
				fmt.Print("|")
			}
			if len(eventType.Counts) > 0 {
				fmt.Printf("_%s_", string(eventType.Counts[0].Category))
			} else {
				fmt.Print("___")
			}
		}
		fmt.Printf("|'%s'\n", line.Action)
	}

	for index := 0; index < length; index++ {
		fmt.Print("---")
	}
	for index := 0; index <= len(dates); index++ {
		fmt.Print("-")
	}
	fmt.Println()
}

func TestEventStringsMultipleEventTypesAndCounts(t *testing.T) {
	var (
		ctx             = context.Background()
		numberOfBuckets = 24 * 3
		timezone        = "America/Los_Angeles"
		loc, _          = time.LoadLocation(timezone)
		endDate         = now.In(loc)
		totalEntries    = 3
	)
	endDate = time.Date(endDate.Year(), endDate.Month(), endDate.Day(),
		12, 30, 0, 0, endDate.Location())

	entries := []*util.FeedEntry{
		// delete - two different entity types
		{
			ID:                 newUUID(),
			ProducerID:         "producerId",
			ProducerName:       "producerName",
			ProducerObjectType: "producerType",
			ProducerTags:       []string{"producerTags"},
			FeedType:           "event",
			EventType:          event_server.ProfileDeleted,
			Tags:               []string{"profile", "delete"},
			Published:          endDate,
			ActorID:            "actorId",
			ActorName:          "actorName",
			ActorObjectType:    "actorType",
			Verb:               "delete",
			ObjectID:           "objectId",
			ObjectName:         "entity_1",
			ObjectObjectType:   "profile",
			TargetID:           "targetId",
			TargetName:         "targetName",
			TargetObjectType:   "targetObjectType",
			Created:            endDate,
		},
		{
			ID:                 newUUID(),
			ProducerID:         "producerId",
			ProducerName:       "producerName",
			ProducerObjectType: "producerType",
			ProducerTags:       []string{"producerTags"},
			FeedType:           "event",
			EventType:          event_server.ScanJobDeleted,
			Tags:               []string{"scanjobs", "delete"},
			Published:          endDate,
			ActorID:            "actorId",
			ActorName:          "actorName",
			ActorObjectType:    "actorType",
			Verb:               "delete",
			ObjectID:           "objectId",
			ObjectName:         "entity_1",
			ObjectObjectType:   "scanjobs",
			TargetID:           "targetId",
			TargetName:         "targetName",
			TargetObjectType:   "targetObjectType",
			Created:            endDate,
		},
		// update - five events of the same type
		{
			ID:                 newUUID(),
			ProducerID:         "producerId",
			ProducerName:       "producerName",
			ProducerObjectType: "producerType",
			ProducerTags:       []string{"producerTags"},
			FeedType:           "event",
			EventType:          event_server.ScanJobUpdated,
			Tags:               []string{"scanjobs", "update"},
			Published:          endDate.AddDate(0, 0, -1),
			ActorID:            "actorId",
			ActorName:          "actorName",
			ActorObjectType:    "actorType",
			Verb:               "update",
			ObjectID:           "objectId",
			ObjectName:         "entity_2",
			ObjectObjectType:   "scanjobs",
			TargetID:           "targetId",
			TargetName:         "targetName",
			TargetObjectType:   "targetObjectType",
			Created:            endDate.AddDate(0, 0, -1),
		},
		{
			ID:                 newUUID(),
			ProducerID:         "producerId",
			ProducerName:       "producerName",
			ProducerObjectType: "producerType",
			ProducerTags:       []string{"producerTags"},
			FeedType:           "event",
			EventType:          event_server.ScanJobUpdated,
			Tags:               []string{"scanjobs", "update"},
			Published:          endDate.AddDate(0, 0, -1),
			ActorID:            "actorId",
			ActorName:          "actorName",
			ActorObjectType:    "actorType",
			Verb:               "update",
			ObjectID:           "objectId",
			ObjectName:         "entity_2",
			ObjectObjectType:   "scanjobs",
			TargetID:           "targetId",
			TargetName:         "targetName",
			TargetObjectType:   "targetObjectType",
			Created:            endDate.AddDate(0, 0, -1),
		},
		{
			ID:                 newUUID(),
			ProducerID:         "producerId",
			ProducerName:       "producerName",
			ProducerObjectType: "producerType",
			ProducerTags:       []string{"producerTags"},
			FeedType:           "event",
			EventType:          event_server.ScanJobUpdated,
			Tags:               []string{"scanjobs", "update"},
			Published:          endDate.AddDate(0, 0, -1),
			ActorID:            "actorId",
			ActorName:          "actorName",
			ActorObjectType:    "actorType",
			Verb:               "update",
			ObjectID:           "objectId",
			ObjectName:         "entity_2",
			ObjectObjectType:   "scanjobs",
			TargetID:           "targetId",
			TargetName:         "targetName",
			TargetObjectType:   "targetObjectType",
			Created:            endDate.AddDate(0, 0, -1),
		},
		{
			ID:                 newUUID(),
			ProducerID:         "producerId",
			ProducerName:       "producerName",
			ProducerObjectType: "producerType",
			ProducerTags:       []string{"producerTags"},
			FeedType:           "event",
			EventType:          event_server.ScanJobUpdated,
			Tags:               []string{"scanjobs", "update"},
			Published:          endDate.AddDate(0, 0, -1),
			ActorID:            "actorId",
			ActorName:          "actorName",
			ActorObjectType:    "actorType",
			Verb:               "update",
			ObjectID:           "objectId",
			ObjectName:         "entity_2",
			ObjectObjectType:   "scanjobs",
			TargetID:           "targetId",
			TargetName:         "targetName",
			TargetObjectType:   "targetObjectType",
			Created:            endDate.AddDate(0, 0, -1),
		},
		{
			ID:                 newUUID(),
			ProducerID:         "producerId",
			ProducerName:       "producerName",
			ProducerObjectType: "producerType",
			ProducerTags:       []string{"producerTags"},
			FeedType:           "event",
			EventType:          event_server.ScanJobUpdated,
			Tags:               []string{"scanjobs", "update"},
			Published:          endDate.AddDate(0, 0, -1),
			ActorID:            "actorId",
			ActorName:          "actorName",
			ActorObjectType:    "actorType",
			Verb:               "update",
			ObjectID:           "objectId",
			ObjectName:         "entity_2",
			ObjectObjectType:   "scanjobs",
			TargetID:           "targetId",
			TargetName:         "targetName",
			TargetObjectType:   "targetObjectType",
			Created:            endDate.AddDate(0, 0, -1),
		},
		// create - mix; two different events twice each
		{
			ID:                 newUUID(),
			ProducerID:         "producerId",
			ProducerName:       "producerName",
			ProducerObjectType: "producerType",
			ProducerTags:       []string{"producerTags"},
			FeedType:           "event",
			EventType:          event_server.ProfileCreated,
			Tags:               []string{"profile", "create"},
			Published:          endDate.AddDate(0, 0, -2),
			ActorID:            "actorId",
			ActorName:          "actorName",
			ActorObjectType:    "actorType",
			Verb:               "create",
			ObjectID:           "objectId",
			ObjectName:         "entity_3",
			ObjectObjectType:   "profile",
			TargetID:           "targetId",
			TargetName:         "targetName",
			TargetObjectType:   "targetObjectType",
			Created:            endDate.AddDate(0, 0, -2),
		},
		{
			ID:                 newUUID(),
			ProducerID:         "producerId",
			ProducerName:       "producerName",
			ProducerObjectType: "producerType",
			ProducerTags:       []string{"producerTags"},
			FeedType:           "event",
			EventType:          event_server.ProfileCreated,
			Tags:               []string{"profile", "create"},
			Published:          endDate.AddDate(0, 0, -2),
			ActorID:            "actorId",
			ActorName:          "actorName",
			ActorObjectType:    "actorType",
			Verb:               "create",
			ObjectID:           "objectId",
			ObjectName:         "entity_3",
			ObjectObjectType:   "profile",
			TargetID:           "targetId",
			TargetName:         "targetName",
			TargetObjectType:   "targetObjectType",
			Created:            endDate.AddDate(0, 0, -2),
		},
		{
			ID:                 newUUID(),
			ProducerID:         "producerId",
			ProducerName:       "producerName",
			ProducerObjectType: "producerType",
			ProducerTags:       []string{"producerTags"},
			FeedType:           "event",
			EventType:          event_server.ScanJobCreated,
			Tags:               []string{"scanjobs", "create"},
			Published:          endDate.AddDate(0, 0, -2),
			ActorID:            "actorId",
			ActorName:          "actorName",
			ActorObjectType:    "actorType",
			Verb:               "create",
			ObjectID:           "objectId",
			ObjectName:         "entity_3",
			ObjectObjectType:   "scanjobs",
			TargetID:           "targetId",
			TargetName:         "targetName",
			TargetObjectType:   "targetObjectType",
			Created:            endDate.AddDate(0, 0, -2),
		},
		{
			ID:                 newUUID(),
			ProducerID:         "producerId",
			ProducerName:       "producerName",
			ProducerObjectType: "producerType",
			ProducerTags:       []string{"producerTags"},
			FeedType:           "event",
			EventType:          event_server.ScanJobCreated,
			Tags:               []string{"scanjobs", "create"},
			Published:          endDate.AddDate(0, 0, -2),
			ActorID:            "actorId",
			ActorName:          "actorName",
			ActorObjectType:    "actorType",
			Verb:               "create",
			ObjectID:           "objectId",
			ObjectName:         "entity_3",
			ObjectObjectType:   "scanjobs",
			TargetID:           "targetId",
			TargetName:         "targetName",
			TargetObjectType:   "targetObjectType",
			Created:            endDate.AddDate(0, 0, -2),
		},
	}

	for _, entry := range entries {
		testSuite.feedBackend.CreateFeedEntry(entry)
	}

	testSuite.RefreshIndices(persistence.IndexNameFeeds)
	defer GetTestSuite().DeleteAllDocuments()

	request := &event_feed.FeedTimelineRequest{
		Interval: 1,
		Start:    endDate.AddDate(0, 0, -2).Format("2006-01-02"),
		End:      endDate.Format("2006-01-02"),
		Timezone: timezone,
	}

	feedTimeline, err := testSuite.feedServer.GetFeedTimeline(ctx, request)

	assert.Nil(t, err)
	totalItems := 0
	for _, line := range feedTimeline.ActionLines {
		assert.Equal(t, numberOfBuckets, len(line.Slots))
		totalItems = totalItems + countItems(line)
		if line.Action == "create" {
			for index, item := range line.Slots {
				if index == 12 {
					assert.Equal(t, 2, len(item.Counts))
					assert.Equal(t, "profile", item.Counts[0].Category)
					assert.Equal(t, int64(2), item.Counts[0].Count)
					assert.Equal(t, "scanjobs", item.Counts[1].Category)
					assert.Equal(t, int64(2), item.Counts[1].Count)
				} else {
					assert.Equal(t, 0, len(item.Counts))
				}
			}
		}
		if line.Action == "update" {
			for index, item := range line.Slots {
				if index == 36 {
					assert.Equal(t, 1, len(item.Counts))
					assert.Equal(t, "scanjobs", item.Counts[0].Category)
					assert.Equal(t, int64(5), item.Counts[0].Count)
				} else {
					assert.Equal(t, 0, len(item.Counts))
				}
			}
		}
		if line.Action == "delete" {
			for index, item := range line.Slots {
				if index == 60 {
					assert.Equal(t, 2, len(item.Counts))
					assert.Equal(t, "profile", item.Counts[0].Category)
					assert.Equal(t, int64(1), item.Counts[0].Count)
					assert.Equal(t, "scanjobs", item.Counts[1].Category)
					assert.Equal(t, int64(1), item.Counts[1].Count)
				} else {
					assert.Equal(t, 0, len(item.Counts))
				}
			}
		}
	}

	assert.Equal(t, totalEntries, totalItems)
	assert.Equal(t, request.Start, feedTimeline.Start)
	assert.Equal(t, request.End, feedTimeline.End)

	printFeedTimeline(feedTimeline, request)
}
