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

	event_server "github.com/chef/automate/components/event-service/server"

	"github.com/chef/automate/api/interservice/event_feed"
	"github.com/chef/automate/components/event-feed-service/pkg/persistence"
	"github.com/chef/automate/components/event-feed-service/pkg/server"
	"github.com/chef/automate/components/event-feed-service/pkg/util"

	log "github.com/sirupsen/logrus"
	"google.golang.org/grpc/codes"

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

			_, err := testSuite.feedServer.GetFeed(ctx, &test.request)

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
			EventType:          event_server.ScanJobUpdated,
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
				res, err := testSuite.feedServer.GetFeed(ctx, &test.request)
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
			EventType:          event_server.ScanJobUpdated,
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
			description: "should return all 10 events (default)",
			request: event_feed.FeedRequest{
				Size: pageSize,
			},
			expected: expectedEntries,
		},
		{
			description: "should return only 'org_2' and profiles events",
			request: event_feed.FeedRequest{
				Filters: []string{"org_name:org_2", "entity_type:profiles"},
				Size:    pageSize,
			},
			expected: expectedEntries[0:5],
		},
		{
			description: "should return only 'org_1' organization events",
			request: event_feed.FeedRequest{
				Filters: []string{"org_name:org_1"},
				Size:    pageSize,
			},
			expected: expectedEntries[10:15],
		},
		{
			description: "should return only 'org_2' organization events",
			request: event_feed.FeedRequest{
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
				res, err := testSuite.feedServer.GetFeed(ctx, &test.request)
				if assert.Nil(t, err) {
					expectedEvents := &event_feed.FeedResponse{FeedEntries: test.expected, TotalEntries: int64(len(test.expected))}
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
			eventType        = event_server.ScanJobUpdated
			tags             = []string{"org_1", "compliance", eventTypes[1]}
			verb             = "update"
			objectID         = "urn:chef:compliance:scan-job"
			objectObjectType = "ScanJob"
			objectName       = "Scan Job"
		)

		if i > 5 {
			name = "Violet"
			userURN = "urn:mycompany:user:violet"
			eventType = event_server.ProfileCreated
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
				Filters: []string{"entity_type:" + eventTypes[0]},
				Size:    pageSize,
			},
			expected: expectedEntries[0:6],
		},
		{
			description: "should return only 6 scan job type events",
			request: event_feed.FeedRequest{
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
				res, err := testSuite.feedServer.GetFeed(ctx, &test.request)
				if assert.Nil(t, err) {
					expectedEvents := &event_feed.FeedResponse{FeedEntries: test.expected, TotalEntries: int64(len(test.expected))}
					assert.Equal(t, expectedEvents, res)
				}
			})
	}
}

func toMilliseconds(timestamp *google_protobuf.Timestamp) int64 {
	return timestamp.GetSeconds()*millisPerSecond +
		int64(timestamp.GetNanos()/nanosPerMillisecond)
}
