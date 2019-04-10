package integration_test

import (
	"context"
	"fmt"
	"math/rand"
	"strconv"
	"testing"
	"time"

	"github.com/stretchr/testify/assert"
	"google.golang.org/grpc/codes"

	"github.com/chef/automate/api/interservice/cfgmgmt/request"
	"github.com/chef/automate/api/interservice/cfgmgmt/response"
	iBackend "github.com/chef/automate/components/ingest-service/backend"
	"github.com/chef/automate/lib/grpc/grpctest"
)

type testCase struct {
	description    string
	request        request.EventCountsFilter
	expectedCounts *response.EventCounts
}

func TestEventCountReturnErrorWithWrongParameters(t *testing.T) {
	var (
		ctx  = context.Background()
		date = time.Now()
	)

	_, err := cfgmgmt.GetEventTypeCounts(ctx, &request.EventCountsFilter{
		End:   date.AddDate(0, 0, -6).Unix() * 1000,
		Start: date.Unix() * 1000,
	})

	grpctest.AssertCode(t, codes.InvalidArgument, err)
}

func TestEventCountsReturnOnlyEventsWithinDateRange(t *testing.T) {
	var (
		totalActions = 10
		startDate    = time.Now()
		timeDiff     = int(time.Hour) * -24
		actions      = createActions(startDate, totalActions, timeDiff)
	)

	suite.IngestActions(actions)
	defer suite.DeleteAllDocuments()

	cases := []testCase{
		{
			description:    "should count 10 events (default)",
			request:        request.EventCountsFilter{},
			expectedCounts: actionsToEventCounts(actions),
		},
		{
			description: "should count all 10 events",
			request: request.EventCountsFilter{
				Start: startDate.AddDate(0, 0, -11).Unix() * 1000,
			},
			expectedCounts: actionsToEventCounts(actions),
		},
		{
			description: "should count only first event",
			request: request.EventCountsFilter{
				End: startDate.AddDate(0, 0, -1).Unix() * 1000,
			},
			expectedCounts: actionsToEventCounts(actions[1:10]),
		},
		{
			description: "should count only one event",
			request: request.EventCountsFilter{
				Start: startDate.AddDate(0, 0, -5).Unix() * 1000,
				End:   startDate.AddDate(0, 0, -5).Unix() * 1000,
			},
			expectedCounts: actionsToEventCounts(actions[5:6]),
		},
	}

	// Run all the cases!
	runCases(t, cases)
}

func TestEventCountsReturnCountOverAThousandActions(t *testing.T) {
	var (
		ctx          = context.Background()
		totalActions = 1001
		startDate    = time.Now()
		timeDiff     = int(time.Minute) * -1
		actions      = createActions(startDate, totalActions, timeDiff)
		request      = request.EventCountsFilter{
			Start: startDate.AddDate(0, 0, -11).Unix() * 1000,
		}
		expectedCounts = actionsToEventCounts(actions)
	)
	suite.IngestActions(actions)
	defer suite.DeleteAllDocuments()

	t.Run("Test to see if 1001 actions were counted",
		func(t *testing.T) {
			res, err := cfgmgmt.GetEventTypeCounts(ctx, &request)
			if assert.Nil(t, err) {
				assert.Equal(t, int64(1001), res.Total)

				assert.Equal(t, len(expectedCounts.Counts), len(res.Counts),
					"Expected number of Counts does not match results %d != %d", len(expectedCounts.Counts), len(res.Counts))

				for _, count := range expectedCounts.Counts {
					foundCount, found := findCount(res.Counts, count)
					assert.True(t, found, "Count %s was not found in results", count.Name)

					assert.Equal(t, count.Count, foundCount.Count,
						"Expected number of Counts does not match results %s = %s %d != %d",
						count.Name, foundCount.Name, count.Count, foundCount.Count)
				}
			}
		})
}

func TestEventCountsCountOnlyFilteredOrgs(t *testing.T) {
	var (
		totalActions = 24
		startDate    = time.Now()
		timeDiff     = int(time.Hour) * -2
		actions      = createActions(startDate, totalActions, timeDiff)
	)

	suite.IngestActions(actions)
	defer suite.DeleteAllDocuments()

	cases := []testCase{
		{
			description: "should count only 'org_1' actions",
			request: request.EventCountsFilter{
				Filter: []string{"organization:org_1"},
			},
			expectedCounts: actionsToEventCounts(filter(actions, func(a iBackend.InternalChefAction) bool {
				return a.OrganizationName == "org_1"
			})),
		},
		{
			description: "should count only 'org_4' actions",
			request: request.EventCountsFilter{
				Filter: []string{"organization:org_4"},
			},
			expectedCounts: actionsToEventCounts(filter(actions, func(a iBackend.InternalChefAction) bool {
				return a.OrganizationName == "org_4"
			})),
		},
		{
			description: "should count only 'org_1' and 'org_2' actions",
			request: request.EventCountsFilter{
				Filter: []string{"organization:org_1", "organization:org_2"},
			},
			expectedCounts: actionsToEventCounts(filter(actions, func(a iBackend.InternalChefAction) bool {
				return a.OrganizationName == "org_1" || a.OrganizationName == "org_2"
			})),
		},
	}

	// Run all the cases!
	runCases(t, cases)
}

func TestEventCountsCountOnlyFilteredChefServers(t *testing.T) {
	var (
		totalActions = 24
		startDate    = time.Now()
		timeDiff     = int(time.Hour) * -2
		actions      = createActions(startDate, totalActions, timeDiff)
	)

	suite.IngestActions(actions)
	defer suite.DeleteAllDocuments()

	cases := []testCase{
		{
			description: "should count only actions from 'chef1' chef server",
			request: request.EventCountsFilter{
				Filter: []string{"source_fqdn:chef1"},
			},
			expectedCounts: actionsToEventCounts(filter(actions, func(a iBackend.InternalChefAction) bool {
				return a.ServiceHostname == "chef1"
			})),
		},
		{
			description: "should count only actions from 'chef2' chef server",
			request: request.EventCountsFilter{
				Filter: []string{"source_fqdn:chef2"},
			},
			expectedCounts: actionsToEventCounts(filter(actions, func(a iBackend.InternalChefAction) bool {
				return a.ServiceHostname == "chef2"
			})),
		},
		{
			description: "should count only actions from 'chef1' and 'chef2' chef servers",
			request: request.EventCountsFilter{
				Filter: []string{"source_fqdn:chef1", "source_fqdn:chef2"},
			},
			expectedCounts: actionsToEventCounts(filter(actions, func(a iBackend.InternalChefAction) bool {
				return a.ServiceHostname == "chef1" || a.ServiceHostname == "chef2"
			})),
		},
	}

	// Run all the cases!
	runCases(t, cases)
}

func runCases(t *testing.T, cases []testCase) {
	ctx := context.Background()
	for _, test := range cases {
		t.Run(fmt.Sprintf("with request '%v' it %s", test.request, test.description),
			func(t *testing.T) {
				res, err := cfgmgmt.GetEventTypeCounts(ctx, &test.request)
				if assert.Nil(t, err) {
					assert.Equal(t, test.expectedCounts.Total, res.Total)

					assert.Equal(t, len(test.expectedCounts.Counts), len(res.Counts),
						"Expected number of Counts does not match results %d != %d", len(test.expectedCounts.Counts), len(res.Counts))

					for _, count := range test.expectedCounts.Counts {
						foundCount, found := findCount(res.Counts, count)

						assert.True(t, found, "Count %s was not found in results", count.Name)

						if found {
							assert.Equal(t, count.Count, foundCount.Count,
								"Expected number of Counts does not match results %s = %s %d != %d",
								count.Name, foundCount.Name, count.Count, foundCount.Count)
						}
					}
				}
			})
	}
}

func filter(actions []iBackend.InternalChefAction, f func(iBackend.InternalChefAction) bool) []iBackend.InternalChefAction {
	filteredActions := make([]iBackend.InternalChefAction, 0)
	for _, action := range actions {
		if f(action) {
			filteredActions = append(filteredActions, action)
		}
	}
	return filteredActions
}

func createActions(startDate time.Time, amountToCreate int, timeDiff int) []iBackend.InternalChefAction {
	var (
		actions     = []iBackend.InternalChefAction{}
		entityTypes = []string{"cookbook", "bag", "item", "scan job", "node", "client"}
		orgs        = []string{"org1", "org2", "org3", "org4"}
		chefServers = []string{"chef1", "chef2", "chef3", "chef4"}
		tasks       = []string{"create", "update", "delete"}
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
}

func findCount(counts []*response.EventCount, matchingCount *response.EventCount) (*response.EventCount, bool) {
	for _, count := range counts {
		if count.Name == matchingCount.Name {
			return count, true
		}
	}

	return nil, false
}

func actionsToEventCounts(actions []iBackend.InternalChefAction) *response.EventCounts {
	eventTypeCount := map[string]int{}

	for _, action := range actions {
		_, found := eventTypeCount[action.EntityType]
		if found {
			eventTypeCount[action.EntityType]++
		} else {
			eventTypeCount[action.EntityType] = 1
		}
	}

	eventCounts := make([]*response.EventCount, len(eventTypeCount))

	index := 0
	for key, value := range eventTypeCount {
		eventCounts[index] = &response.EventCount{
			Name:  key,
			Count: int64(value),
		}
		index++
	}

	return &response.EventCounts{
		Total:  int64(len(actions)),
		Counts: eventCounts,
	}
}
