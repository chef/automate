package integration_test

import (
	"context"
	"fmt"
	"math"
	"math/rand"
	"strconv"
	"testing"
	"time"

	"github.com/golang/protobuf/ptypes"
	google_protobuf "github.com/golang/protobuf/ptypes/timestamp"
	"github.com/stretchr/testify/assert"
	"google.golang.org/grpc/codes"

	"github.com/chef/automate/api/interservice/cfgmgmt/request"
	"github.com/chef/automate/api/interservice/cfgmgmt/response"
	iBackend "github.com/chef/automate/components/ingest-service/backend"
	"github.com/chef/automate/lib/grpc/grpctest"
)

var (
	millisPerSecond     = int64(1000)
	nanosPerMillisecond = int32(time.Millisecond)
)

func TestEventFeedReturnErrorWithWrongParameters(t *testing.T) {
	var (
		ctx  = context.Background()
		date = time.Now()
	)
	cases := []struct {
		description string
		request     request.EventFilter
	}{
		{
			description: "The Start date is after the End date",
			request: request.EventFilter{
				End:      date.AddDate(0, 0, -6).Unix() * 1000,
				Start:    date.Unix() * 1000,
				PageSize: 10,
			},
		},
		{
			description: "Before and After parameters should not both be set",
			request: request.EventFilter{
				End:      date.Unix() * 1000,
				Start:    date.AddDate(0, 0, -6).Unix() * 1000,
				PageSize: 10,
				Before:   date.AddDate(0, 0, -3).Unix() * 1000,
				After:    date.AddDate(0, 0, -1).Unix() * 1000,
			},
		},
		{
			description: "If the Before param is set the Cursor param needs to be set also",
			request: request.EventFilter{
				End:      date.Unix() * 1000,
				Start:    date.AddDate(0, 0, -6).Unix() * 1000,
				PageSize: 10,
				Before:   date.AddDate(0, 0, -3).Unix() * 1000,
			},
		},
		{
			description: "When the After is set without the Cursor, After must be equal to End",
			request: request.EventFilter{
				End:      date.Unix() * 1000,
				Start:    date.AddDate(0, 0, -6).Unix() * 1000,
				PageSize: 10,
				After:    date.AddDate(0, 0, -3).Unix() * 1000,
			},
		},
	}

	for _, test := range cases {
		t.Run(fmt.Sprintf("with parameters filters=%v it %s should return an error",
			test.request, test.description), func(t *testing.T) {

			_, err := cfgmgmt.GetEventFeed(ctx, &test.request)

			grpctest.AssertCode(t, codes.InvalidArgument, err)
		})
	}
}

func TestEventFeedReturnOnlyEventsWithinDateRange(t *testing.T) {
	var (
		ctx          = context.Background()
		totalActions = 10
		pageSize     = int32(totalActions)
		startDate    = time.Now()
		actions      = []iBackend.InternalChefAction{}
	)

	for i := 0; i < totalActions; i++ {
		var (
			id   = newUUID()
			time = startDate.AddDate(0, 0, i*-1)
			name = "action_" + strconv.Itoa(i)
		)

		data := iBackend.InternalChefAction{
			Id:         id,
			RecordedAt: time,
			EntityName: name,
		}
		actions = append(actions, data)
	}

	suite.IngestActions(actions)
	defer suite.DeleteAllDocuments()
	expectedEvents := actionsToEvents(actions)
	cases := []struct {
		description string
		request     request.EventFilter
		expected    []*response.Event
	}{
		{
			description: "should return all 10 events (default)",
			request: request.EventFilter{
				PageSize: pageSize,
			},
			expected: expectedEvents,
		},
		{
			description: "should return all 10 events",
			request: request.EventFilter{
				Start:    startDate.AddDate(0, 0, -10).Unix() * 1000,
				PageSize: pageSize,
			},
			expected: expectedEvents,
		},
		{
			description: "should return first 5 events",
			request: request.EventFilter{
				Start:    startDate.AddDate(0, 0, -5).Unix() * 1000,
				PageSize: pageSize,
			},
			expected: expectedEvents[0:6],
		},
		{
			description: "should return last 5 events",
			request: request.EventFilter{
				End:      startDate.AddDate(0, 0, -5).Unix() * 1000,
				PageSize: pageSize,
			},
			expected: expectedEvents[5:10],
		},
		{
			description: "should return one event",
			request: request.EventFilter{
				Start:    startDate.AddDate(0, 0, -5).Unix() * 1000,
				End:      startDate.AddDate(0, 0, -5).Unix() * 1000,
				PageSize: pageSize,
			},
			expected: expectedEvents[5:6],
		},
		{
			description: "dates after should return zero events",
			request: request.EventFilter{
				Start:    startDate.AddDate(0, 0, 1).Unix() * 1000,
				End:      startDate.AddDate(0, 0, 5).Unix() * 1000,
				PageSize: pageSize,
			},
			expected: []*response.Event{},
		},
		{
			description: "dates before should return zero events",
			request: request.EventFilter{
				Start:    startDate.AddDate(0, 0, -11).Unix() * 1000,
				End:      startDate.AddDate(0, 0, -10).Unix() * 1000,
				PageSize: pageSize,
			},
			expected: []*response.Event{},
		},
	}

	// Run all the cases!
	for _, test := range cases {
		t.Run(fmt.Sprintf("with request '%v' it %s", test.request, test.description),
			func(t *testing.T) {
				res, err := cfgmgmt.GetEventFeed(ctx, &test.request)
				if assert.Nil(t, err) {
					assert.Equal(t, len(test.expected), len(res.Events))
					for index, expectedEvent := range test.expected {
						assert.Equal(t, expectedEvent.Timestamp, res.Events[index].Timestamp)
					}
				}
			})
	}
}

func TestEventFeedFilterOrgs(t *testing.T) {
	var (
		ctx          = context.Background()
		totalActions = 10
		pageSize     = int32(totalActions)
		startDate    = time.Now()
		actions      = []iBackend.InternalChefAction{}
	)

	for i := 0; i < totalActions; i++ {
		var (
			id   = newUUID()
			time = startDate.AddDate(0, 0, i*-1)
			name = "action_" + strconv.Itoa(i)
			orgs = "org_1"
		)

		if i > 4 {
			orgs = "org_2"
		}

		data := iBackend.InternalChefAction{
			Id:               id,
			RecordedAt:       time,
			EntityName:       name,
			OrganizationName: orgs,
		}
		actions = append(actions, data)
	}

	suite.IngestActions(actions)
	defer suite.DeleteAllDocuments()
	expectedEvents := actionsToEvents(actions)
	cases := []struct {
		description string
		request     request.EventFilter
		expected    []*response.Event
	}{
		{
			description: "should return all 10 events (default)",
			request: request.EventFilter{
				PageSize: pageSize,
			},
			expected: expectedEvents,
		},
		{
			description: "should return only 'org_1' organization events",
			request: request.EventFilter{
				Filter:   []string{"organization:org_1"},
				PageSize: pageSize,
			},
			expected: expectedEvents[0:5],
		},
		{
			description: "should return only 'org_2' organization events",
			request: request.EventFilter{
				Filter:   []string{"organization:org_2"},
				PageSize: pageSize,
			},
			expected: expectedEvents[5:10],
		},
	}

	// Run all the cases!
	for _, test := range cases {
		t.Run(fmt.Sprintf("with request '%v' it %s", test.request, test.description),
			func(t *testing.T) {
				res, err := cfgmgmt.GetEventFeed(ctx, &test.request)
				if assert.Nil(t, err) {
					expectedEvents := &response.Events{Events: test.expected, TotalEvents: int64(len(test.expected))}
					assert.Equal(t, expectedEvents, res)
				}
			})
	}
}

func TestEventFeedFilterChefServers(t *testing.T) {
	var (
		ctx          = context.Background()
		totalActions = 10
		pageSize     = int32(totalActions)
		startDate    = time.Now()
		actions      = []iBackend.InternalChefAction{}
	)

	for i := 0; i < totalActions; i++ {
		var (
			id   = newUUID()
			time = startDate.AddDate(0, 0, i*-1)
			name = "action_" + strconv.Itoa(i)
			host = "chef_1"
		)

		if i > 4 {
			host = "chef_2"
		}

		data := iBackend.InternalChefAction{
			Id:              id,
			RecordedAt:      time,
			EntityName:      name,
			ServiceHostname: host,
		}
		actions = append(actions, data)
	}

	suite.IngestActions(actions)
	defer suite.DeleteAllDocuments()
	expectedEvents := actionsToEvents(actions)
	cases := []struct {
		description string
		request     request.EventFilter
		expected    []*response.Event
	}{
		{
			description: "should return all 10 events (default)",
			request: request.EventFilter{
				PageSize: pageSize,
			},
			expected: expectedEvents,
		},
		{
			description: "should return only 'chef_1' chef server events",
			request: request.EventFilter{
				Filter:   []string{"source_fqdn:chef_1"},
				PageSize: pageSize,
			},
			expected: expectedEvents[0:5],
		},
		{
			description: "should return only 'chef_2' chef server events",
			request: request.EventFilter{
				Filter:   []string{"source_fqdn:chef_2"},
				PageSize: pageSize,
			},
			expected: expectedEvents[5:10],
		},
	}

	// Run all the cases!
	for _, test := range cases {
		t.Run(fmt.Sprintf("with request '%v' it %s", test.request, test.description),
			func(t *testing.T) {
				res, err := cfgmgmt.GetEventFeed(ctx, &test.request)
				if assert.Nil(t, err) {
					expectedEvents := &response.Events{Events: test.expected, TotalEvents: int64(len(test.expected))}
					assert.Equal(t, expectedEvents, res)
				}
			})
	}
}

func TestEventFeedFilterEventType(t *testing.T) {
	var (
		ctx          = context.Background()
		totalActions = 12
		pageSize     = int32(totalActions)
		startDate    = time.Now()
		actions      = []iBackend.InternalChefAction{}
		eventTypes   = []string{"policyfile", "node", "cookbook", "bag", "environment", "role"}
	)

	for i := 0; i < totalActions; i++ {
		var (
			id        = newUUID()
			time      = startDate.AddDate(0, 0, i*-1)
			name      = "action_" + strconv.Itoa(i)
			eventType = eventTypes[i%len(eventTypes)]
		)

		data := iBackend.InternalChefAction{
			Id:         id,
			RecordedAt: time,
			EntityName: name,
			EntityType: eventType,
		}
		actions = append(actions, data)
	}

	suite.IngestActions(actions)
	defer suite.DeleteAllDocuments()
	expectedEvents := actionsToEvents(actions)
	cases := []struct {
		description string
		request     request.EventFilter
		expected    []*response.Event
	}{
		{
			description: "should return all 12 events (default)",
			request: request.EventFilter{
				PageSize: pageSize,
			},
			expected: expectedEvents,
		},
		{
			description: "should return only the 2 policyfile type events",
			request: request.EventFilter{
				Filter:   []string{"entity_type:policyfile"},
				PageSize: pageSize,
			},
			expected: []*response.Event{expectedEvents[0], expectedEvents[6]},
		},
		{
			description: "should return only the 2 node type events",
			request: request.EventFilter{
				Filter:   []string{"entity_type:node"},
				PageSize: pageSize,
			},
			expected: []*response.Event{expectedEvents[1], expectedEvents[7]},
		},
		{
			description: "should return only the 2 cookbook type events",
			request: request.EventFilter{
				Filter:   []string{"entity_type:cookbook"},
				PageSize: pageSize,
			},
			expected: []*response.Event{expectedEvents[2], expectedEvents[8]},
		},
		{
			description: "should return only the 2 bag type events",
			request: request.EventFilter{
				Filter:   []string{"entity_type:bag"},
				PageSize: pageSize,
			},
			expected: []*response.Event{expectedEvents[3], expectedEvents[9]},
		},
		{
			description: "should return only the 2 environment type events",
			request: request.EventFilter{
				Filter:   []string{"entity_type:environment"},
				PageSize: pageSize,
			},
			expected: []*response.Event{expectedEvents[4], expectedEvents[10]},
		},
		{
			description: "should return only the 2 role type events",
			request: request.EventFilter{
				Filter:   []string{"entity_type:role"},
				PageSize: pageSize,
			},
			expected: []*response.Event{expectedEvents[5], expectedEvents[11]},
		},
		{
			description: "should return no events",
			request: request.EventFilter{
				Filter:   []string{"entity_type:fake"},
				PageSize: pageSize,
			},
			expected: []*response.Event{},
		},
	}

	// Run all the cases!
	for _, test := range cases {
		t.Run(fmt.Sprintf("with request '%v' it %s", test.request, test.description),
			func(t *testing.T) {
				res, err := cfgmgmt.GetEventFeed(ctx, &test.request)
				if assert.Nil(t, err) {
					expectedEvents := &response.Events{Events: test.expected, TotalEvents: int64(len(test.expected))}
					assert.Equal(t, expectedEvents, res)
				}
			})
	}
}

func TestEventFeedPaginationWalkingFromFirstToLastPages(t *testing.T) {
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

				resPage, err := cfgmgmt.GetEventFeed(ctx, &request.EventFilter{
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
					resPage, err = cfgmgmt.GetEventFeed(ctx, &request.EventFilter{
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
				resPage, err := cfgmgmt.GetEventFeed(ctx, &request.EventFilter{
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
					resPage, err = cfgmgmt.GetEventFeed(ctx, &request.EventFilter{
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

				resPage, err := cfgmgmt.GetEventFeed(ctx, &request.EventFilter{
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
					resPage, err = cfgmgmt.GetEventFeed(ctx, &request.EventFilter{
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

				resPage, err := cfgmgmt.GetEventFeed(ctx, &request.EventFilter{
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

					resPage, err = cfgmgmt.GetEventFeed(ctx, &request.EventFilter{
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

func toMilliseconds(timestamp *google_protobuf.Timestamp) int64 {
	return timestamp.GetSeconds()*millisPerSecond +
		int64(timestamp.GetNanos()/nanosPerMillisecond)
}

func actionsToEvents(actions []iBackend.InternalChefAction) []*response.Event {
	csEvents := make([]*response.Event, len(actions))
	for index, action := range actions {
		timestamp, _ := ptypes.TimestampProto(action.RecordedAt)
		csEvents[index] = &response.Event{
			Id:              action.Id,
			EventType:       action.EntityType,
			Task:            action.Task,
			Timestamp:       timestamp,
			EntityName:      action.EntityName,
			RequestorType:   action.RequestorType,
			RequestorName:   action.RequestorName,
			ServiceHostname: action.ServiceHostname,
			ParentName:      action.ParentName,
			ParentType:      action.ParentType,
		}
	}
	return csEvents
}
