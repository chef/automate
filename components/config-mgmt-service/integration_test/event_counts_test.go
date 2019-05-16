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
	authzConstants "github.com/chef/automate/components/authz-service/constants/v2"
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
					"Expected number of Counts does not match results %d != %d",
					len(expectedCounts.Counts), len(res.Counts))

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

func TestEventTypeCountsProjectFilter(t *testing.T) {
	cases := []struct {
		description string
		actions     []iBackend.InternalChefAction
		ctx         context.Context
		expected    *response.EventCounts
	}{
		{
			description: "No Actions with requesting projects",
			actions:     []iBackend.InternalChefAction{},
			ctx:         contextWithProjects([]string{"project9"}),
			expected: &response.EventCounts{
				Total:  0,
				Counts: []*response.EventCount{},
			},
		},
		{
			description: "One Action with a project matching requested projects",
			actions: []iBackend.InternalChefAction{
				{
					EntityType: "cookbook",
					Projects:   []string{"project9"},
				},
			},
			ctx: contextWithProjects([]string{"project9"}),
			expected: &response.EventCounts{
				Total: 1,
				Counts: []*response.EventCount{
					{
						Name:  "cookbook",
						Count: 1,
					},
				},
			},
		},
		{
			description: "Two Actions with a project matching requested projects",
			actions: []iBackend.InternalChefAction{
				{
					EntityType: "cookbook",
					Projects:   []string{"project9"},
				},
				{
					EntityType: "node",
					Projects:   []string{"project9"},
				},
			},
			ctx: contextWithProjects([]string{"project9"}),
			expected: &response.EventCounts{
				Total: 2,
				Counts: []*response.EventCount{
					{
						Name:  "cookbook",
						Count: 1,
					},
					{
						Name:  "node",
						Count: 1,
					},
				},
			},
		},
		{
			description: "Two Actions with only one's project matching requested projects",
			actions: []iBackend.InternalChefAction{
				{
					EntityType: "cookbook",
					Projects:   []string{"project9"},
				},
				{
					EntityType: "node",
					Projects:   []string{"project3"},
				},
			},
			ctx: contextWithProjects([]string{"project9"}),
			expected: &response.EventCounts{
				Total: 1,
				Counts: []*response.EventCount{
					{
						Name:  "cookbook",
						Count: 1,
					},
				},
			},
		},
		{
			description: "Action project not matching request projects",
			actions: []iBackend.InternalChefAction{
				{
					EntityType: "cookbook",
					Projects:   []string{"project9"},
				},
			},
			ctx: contextWithProjects([]string{"project3"}),
			expected: &response.EventCounts{
				Total:  0,
				Counts: []*response.EventCount{},
			},
		},
		{
			description: "One Action has one project; request all projects allowed",
			ctx:         contextWithProjects([]string{authzConstants.AllProjectsExternalID}),
			actions: []iBackend.InternalChefAction{
				{
					EntityType: "cookbook",
					Projects:   []string{"project9"},
				},
			},
			expected: &response.EventCounts{
				Total: 1,
				Counts: []*response.EventCount{
					{
						Name:  "cookbook",
						Count: 1,
					},
				},
			},
		},
		{
			description: "Three Actions; request all projects allowed",
			ctx:         contextWithProjects([]string{authzConstants.AllProjectsExternalID}),
			actions: []iBackend.InternalChefAction{
				{
					EntityType: "cookbook",
					Projects:   []string{"project9"},
				},
				{
					EntityType: "node",
					Projects:   []string{"project12"},
				},
				{
					EntityType: "bag",
					Projects:   []string{},
				},
			},
			expected: &response.EventCounts{
				Total: 3,
				Counts: []*response.EventCount{
					{
						Name:  "cookbook",
						Count: 1,
					},
					{
						Name:  "node",
						Count: 1,
					},
					{
						Name:  "bag",
						Count: 1,
					},
				},
			},
		},
		{
			description: "Action has no projects; request all projects allowed",
			ctx:         contextWithProjects([]string{authzConstants.AllProjectsExternalID}),
			actions: []iBackend.InternalChefAction{
				{
					EntityType: "cookbook",
					Projects:   []string{},
				},
			},
			expected: &response.EventCounts{
				Total: 1,
				Counts: []*response.EventCount{
					{
						Name:  "cookbook",
						Count: 1,
					},
				},
			},
		},
		{
			description: "Action has no projects; request unassigned projects allowed",
			ctx:         contextWithProjects([]string{authzConstants.UnassignedProjectID}),
			actions: []iBackend.InternalChefAction{
				{
					EntityType: "cookbook",
					Projects:   []string{},
				},
			},
			expected: &response.EventCounts{
				Total: 1,
				Counts: []*response.EventCount{
					{
						Name:  "cookbook",
						Count: 1,
					},
				},
			},
		},
		{
			description: "Action has a project; request only unassigned projects",
			ctx:         contextWithProjects([]string{authzConstants.UnassignedProjectID}),
			actions: []iBackend.InternalChefAction{
				{
					EntityType: "cookbook",
					Projects:   []string{"project9"},
				},
			},
			expected: &response.EventCounts{
				Total:  0,
				Counts: []*response.EventCount{},
			},
		},
		{
			description: "Two Actions have and don't have projects; request only unassigned projects",
			ctx:         contextWithProjects([]string{authzConstants.UnassignedProjectID}),
			actions: []iBackend.InternalChefAction{
				{
					EntityType: "cookbook",
					Projects:   []string{"project9"},
				},
				{
					EntityType: "node",
					Projects:   []string{},
				},
			},
			expected: &response.EventCounts{
				Total: 1,
				Counts: []*response.EventCount{
					{
						Name:  "node",
						Count: 1,
					},
				},
			},
		},
		{
			description: "Action has a project; request unassigned and matching project allowed",
			ctx:         contextWithProjects([]string{authzConstants.UnassignedProjectID, "project9"}),
			actions: []iBackend.InternalChefAction{
				{
					EntityType: "cookbook",
					Projects:   []string{"project9"},
				},
			},
			expected: &response.EventCounts{
				Total: 1,
				Counts: []*response.EventCount{
					{
						Name:  "cookbook",
						Count: 1,
					},
				},
			},
		},
		{
			description: "Action has no projects; request has no projects",
			ctx:         contextWithProjects([]string{}),
			actions: []iBackend.InternalChefAction{
				{
					EntityType: "cookbook",
					Projects:   []string{},
				},
			},
			expected: &response.EventCounts{
				Total: 1,
				Counts: []*response.EventCount{
					{
						Name:  "cookbook",
						Count: 1,
					},
				},
			},
		},
		{
			description: "Two Actions have and don't have projects; request has no projects",
			ctx:         contextWithProjects([]string{}),
			actions: []iBackend.InternalChefAction{
				{
					EntityType: "cookbook",
					Projects:   []string{"project9"},
				},
				{
					EntityType: "node",
					Projects:   []string{},
				},
			},
			expected: &response.EventCounts{
				Total: 2,
				Counts: []*response.EventCount{
					{
						Name:  "node",
						Count: 1,
					},
					{
						Name:  "cookbook",
						Count: 1,
					},
				},
			},
		},
		{
			description: "Action with one project matching one of several requested projects allowed",
			ctx:         contextWithProjects([]string{"project3", "project9", "project7", "project6"}),
			actions: []iBackend.InternalChefAction{
				{
					EntityType: "cookbook",
					Projects:   []string{"project9"},
				},
			},
			expected: &response.EventCounts{
				Total: 1,
				Counts: []*response.EventCount{
					{
						Name:  "cookbook",
						Count: 1,
					},
				},
			},
		},
		{
			description: "Two actions with one project matching different one of several requested projects allowed",
			ctx:         contextWithProjects([]string{"project3", "project9", "project7", "project6"}),
			actions: []iBackend.InternalChefAction{
				{
					EntityType: "cookbook",
					Projects:   []string{"project9"},
				},
				{
					EntityType: "node",
					Projects:   []string{"project3"},
				},
			},
			expected: &response.EventCounts{
				Total: 2,
				Counts: []*response.EventCount{
					{
						Name:  "node",
						Count: 1,
					},
					{
						Name:  "cookbook",
						Count: 1,
					},
				},
			},
		},
		{
			description: "Action with one project not matching any of several requested projects allowed",
			ctx:         contextWithProjects([]string{"project3", "project4", "project7", "project6"}),
			actions: []iBackend.InternalChefAction{
				{
					EntityType: "cookbook",
					Projects:   []string{"project9"},
				},
			},
			expected: &response.EventCounts{
				Total:  0,
				Counts: []*response.EventCount{},
			},
		},
		{
			description: "Two Actions with neither project not matching any of several requested projects allowed",
			ctx:         contextWithProjects([]string{"project3", "project4", "project7", "project6"}),
			actions: []iBackend.InternalChefAction{
				{
					EntityType: "cookbook",
					Projects:   []string{"project9"},
				},
				{
					EntityType: "node",
					Projects:   []string{"project10"},
				},
			},
			expected: &response.EventCounts{
				Total:  0,
				Counts: []*response.EventCount{},
			},
		},
		{
			description: "Action with several projects where one matches a single requested project allowed",
			ctx:         contextWithProjects([]string{"project3"}),
			actions: []iBackend.InternalChefAction{
				{
					EntityType: "cookbook",
					Projects:   []string{"project3", "project4", "project7", "project6"},
				},
			},
			expected: &response.EventCounts{
				Total: 1,
				Counts: []*response.EventCount{
					{
						Name:  "cookbook",
						Count: 1,
					},
				},
			},
		},
		{
			description: "Two Actions with several projects where one matches a single requested project allowed",
			ctx:         contextWithProjects([]string{"project3"}),
			actions: []iBackend.InternalChefAction{
				{
					EntityType: "cookbook",
					Projects:   []string{"project3", "project4", "project7", "project6"},
				},
				{
					EntityType: "node",
					Projects:   []string{"project12", "project10", "project11", "project3"},
				},
			},
			expected: &response.EventCounts{
				Total: 2,
				Counts: []*response.EventCount{
					{
						Name:  "node",
						Count: 1,
					},
					{
						Name:  "cookbook",
						Count: 1,
					},
				},
			},
		},
		{
			description: "Two Actions with several projects where only one action's project matches a single requested project allowed",
			ctx:         contextWithProjects([]string{"project3"}),
			actions: []iBackend.InternalChefAction{
				{
					EntityType: "cookbook",
					Projects:   []string{"project3", "project4", "project7", "project6"},
				},
				{
					EntityType: "node",
					Projects:   []string{"project12", "project10", "project11", "project13"},
				},
			},
			expected: &response.EventCounts{
				Total: 1,
				Counts: []*response.EventCount{
					{
						Name:  "cookbook",
						Count: 1,
					},
				},
			},
		},
		{
			description: "Action with several projects where one matches one of several requested project allowed",
			ctx:         contextWithProjects([]string{"project3", "project10", "project12", "project13"}),
			actions: []iBackend.InternalChefAction{
				{
					EntityType: "cookbook",
					Projects:   []string{"project3", "project4", "project7", "project6"},
				},
			},
			expected: &response.EventCounts{
				Total: 1,
				Counts: []*response.EventCount{
					{
						Name:  "cookbook",
						Count: 1,
					},
				},
			},
		},
		{
			description: "Two Actions with several projects where one matches one of several requested project allowed",
			ctx:         contextWithProjects([]string{"project3", "project10", "project12", "project13"}),
			actions: []iBackend.InternalChefAction{
				{
					EntityType: "cookbook",
					Projects:   []string{"project3", "project4", "project7", "project6"},
				},
				{
					EntityType: "node",
					Projects:   []string{"project13", "project14", "project17", "project16"},
				},
			},
			expected: &response.EventCounts{
				Total: 2,
				Counts: []*response.EventCount{
					{
						Name:  "node",
						Count: 1,
					},
					{
						Name:  "cookbook",
						Count: 1,
					},
				},
			},
		},
		{
			description: "Action with several projects where none matches several requested project allowed",
			ctx:         contextWithProjects([]string{"project14", "project10", "project12", "project13"}),
			actions: []iBackend.InternalChefAction{
				{
					EntityType: "cookbook",
					Projects:   []string{"project3", "project4", "project7", "project6"},
				},
			},
			expected: &response.EventCounts{},
		},
		{
			description: "Action with several projects where two matches two of several requested project allowed",
			ctx:         contextWithProjects([]string{"project3", "project10", "project12", "project13"}),
			actions: []iBackend.InternalChefAction{
				{
					EntityType: "cookbook",
					Projects:   []string{"project3", "project10", "project7", "project6"},
				},
			},
			expected: &response.EventCounts{
				Total: 1,
				Counts: []*response.EventCount{
					{
						Name:  "cookbook",
						Count: 1,
					},
				},
			},
		},
	}

	for _, test := range cases {
		t.Run(fmt.Sprintf("Project filter: %s", test.description), func(t *testing.T) {
			for index := range test.actions {
				test.actions[index].RecordedAt = time.Now()
				test.actions[index].Id = newUUID()
			}

			suite.IngestActions(test.actions)
			defer suite.DeleteAllDocuments()

			res, err := cfgmgmt.GetEventTypeCounts(test.ctx, &request.EventCountsFilter{})
			assert.NoError(t, err)

			// test response
			assert.Equal(t, test.expected.Total, res.Total)
			assert.ElementsMatch(t, test.expected.Counts, res.Counts)
		})
	}
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
						"Expected number of Counts does not match results %d != %d",
						len(test.expectedCounts.Counts), len(res.Counts))

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

func filter(actions []iBackend.InternalChefAction,
	f func(iBackend.InternalChefAction) bool) []iBackend.InternalChefAction {
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
