package integration_test

import (
	"context"
	"fmt"
	"math"
	"strconv"
	"testing"
	"time"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
	"google.golang.org/grpc/codes"

	"github.com/chef/automate/api/interservice/cfgmgmt/request"
	"github.com/chef/automate/api/interservice/cfgmgmt/response"
	authzConstants "github.com/chef/automate/components/authz-service/constants/v2"
	iBackend "github.com/chef/automate/components/ingest-service/backend"
	"github.com/chef/automate/lib/grpc/grpctest"
)

func TestEventStringsStaticDate(t *testing.T) {
	var staticDate = time.Date(2018, time.February, 2, 12, 34, 56, 0, time.UTC)

	testEventStringsWithDate(t, staticDate, "some random date in February")
}

// func TestEventStringsDateNow(t *testing.T) {
// 	var now = time.Now()

// 	testEventStringsWithDate(t, now, "now")
// }

func TestEventStringsStartOfDaylightSavingsDate(t *testing.T) {
	var startDaylightSavings = time.Date(2018, time.March, 12, 12, 34, 56, 0, time.UTC)

	testEventStringsWithDate(t, startDaylightSavings, "start of daylight savings time")
}

func TestEventStringsDateEndOfDaylightSavingsDate(t *testing.T) {
	var endOfDaylightSavings = time.Date(2018, time.November, 5, 12, 34, 56, 0, time.UTC)

	testEventStringsWithDate(t, endOfDaylightSavings, "end of daylight savings time")
}

func TestEventStringsIncorrectHoursBetween(t *testing.T) {
	var ctx = context.Background()
	cases := []int{-1, 0, 5, 7, 9, 10, 11, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 25, 27, 98}

	for _, bucketSize := range cases {
		t.Run(fmt.Sprintf("with parameters hours between=%d it should return an error",
			bucketSize), func(t *testing.T) {

			_, err := cfgmgmt.GetEventStringBuckets(ctx, &request.EventStrings{
				Start:        "2018-01-01",
				End:          "2018-01-06",
				HoursBetween: int32(bucketSize),
				Timezone:     "UTC",
			})

			grpctest.AssertCode(t, codes.InvalidArgument, err)
		})
	}
}

func TestEventStringsIncorrectTimezone(t *testing.T) {
	var ctx = context.Background()
	cases := []request.EventStrings{
		{
			Start:        "2018-01-01",
			End:          "2018-01-06",
			HoursBetween: 3,
			Timezone:     "",
		},
		{
			Start:        "2018-01-01",
			End:          "2018-01-06",
			HoursBetween: 3,
			Timezone:     "fake",
		},
	}

	for _, test := range cases {
		t.Run(fmt.Sprintf("with parameters timezone=%s it should return an error",
			test.Timezone), func(t *testing.T) {

			_, err := cfgmgmt.GetEventStringBuckets(ctx, &test)

			grpctest.AssertCode(t, codes.InvalidArgument, err)
		})
	}
}

func TestEventStringsIncorrectTimes(t *testing.T) {
	var ctx = context.Background()
	cases := []request.EventStrings{
		{
			Start:        "2018-01-06",
			End:          "2018-01-01",
			HoursBetween: 3,
			Timezone:     "UTC",
		},
		{
			Start:        "2000-00-00",
			End:          "2000-00-06",
			HoursBetween: 3,
			Timezone:     "UTC",
		},
		{
			Start:        "00-00-00",
			End:          "00-00-06",
			HoursBetween: 3,
			Timezone:     "UTC",
		},
		{
			Start:        "18-10-10",
			End:          "18-10-16",
			HoursBetween: 3,
			Timezone:     "UTC",
		},
		{
			Start:        "20-01-01",
			End:          "20-01-06",
			HoursBetween: 3,
			Timezone:     "UTC",
		},
		{
			Start:        "17:01:01",
			End:          "17:01:06",
			HoursBetween: 3,
			Timezone:     "UTC",
		},
		{
			Start:        "01-01-2000",
			End:          "06-01-2000",
			HoursBetween: 3,
			Timezone:     "UTC",
		},
		{
			Start:        "3000-12",
			End:          "3006-12",
			HoursBetween: 3,
			Timezone:     "UTC",
		},

		{
			Start:        "2019",
			End:          "2018",
			HoursBetween: 3,
			Timezone:     "UTC",
		},
		{
			Start:        "1888:01:01",
			End:          "1888:01:06",
			HoursBetween: 3,
			Timezone:     "UTC",
		},
		{
			Start:        "2027/01/01",
			End:          "2027/01/08",
			HoursBetween: 3,
			Timezone:     "UTC",
		},
	}

	for _, test := range cases {
		t.Run(fmt.Sprintf("with parameters start=%s end=%s it should return an error",
			test.Start, test.End), func(t *testing.T) {

			_, err := cfgmgmt.GetEventStringBuckets(ctx, &test)

			grpctest.AssertCode(t, codes.InvalidArgument, err)
		})
	}
}

func TestEventStringsProjectFilter(t *testing.T) {
	var (
		timezone  = "UTC"
		startDate = time.Now().AddDate(0, 0, -3)
		endDate   = time.Now()
		request   = request.EventStrings{
			HoursBetween: 1,
			Start:        startDate.Format("2006-01-02"),
			End:          endDate.Format("2006-01-02"),
			Timezone:     timezone,
		}
	)

	cases := []struct {
		description string
		actions     []iBackend.InternalChefAction
		ctx         context.Context
		expected    map[string]int
	}{
		{
			description: "No Actions with requesting projects",
			actions:     []iBackend.InternalChefAction{},
			ctx:         contextWithProjects([]string{"project9"}),
			expected:    map[string]int{},
		},
		{
			description: "One Action with a project matching requested projects",
			actions: []iBackend.InternalChefAction{
				{
					EntityType: "policyfile",
					Task:       "update",
					Projects:   []string{"project9"},
				},
			},
			ctx: contextWithProjects([]string{"project9"}),
			expected: map[string]int{
				"policyfile": 1,
			},
		},
		{
			description: "Two Actions with a project matching requested projects",
			actions: []iBackend.InternalChefAction{
				{
					EntityType: "policyfile",
					Task:       "delete",
					Projects:   []string{"project9"},
				},
				{
					EntityType: "cookbook",
					Task:       "update",
					Projects:   []string{"project9"},
				},
			},
			ctx: contextWithProjects([]string{"project9"}),
			expected: map[string]int{
				"policyfile": 1,
				"cookbook":   1,
			},
		},
		{
			description: "Two Actions with only one's project matching requested projects",
			actions: []iBackend.InternalChefAction{
				{
					EntityType: "policyfile",
					Task:       "update",
					Projects:   []string{"project9"},
				},
				{
					EntityType: "cookbook",
					Task:       "delete",
					Projects:   []string{"project3"},
				},
			},
			ctx: contextWithProjects([]string{"project9"}),
			expected: map[string]int{
				"policyfile": 1,
			},
		},
		{
			description: "Action project not matching request projects",
			actions: []iBackend.InternalChefAction{
				{
					EntityType: "policyfile",
					Task:       "update",
					Projects:   []string{"project9"},
				},
			},
			ctx:      contextWithProjects([]string{"project3"}),
			expected: map[string]int{},
		},
		{
			description: "One Action has one project; request all projects allowed",
			ctx:         contextWithProjects([]string{authzConstants.AllProjectsExternalID}),
			actions: []iBackend.InternalChefAction{
				{
					EntityType: "policyfile",
					Task:       "update",
					Projects:   []string{"project9"},
				},
			},
			expected: map[string]int{
				"policyfile": 1,
			},
		},
		{
			description: "Three Actions; request all projects allowed",
			ctx:         contextWithProjects([]string{authzConstants.AllProjectsExternalID}),
			actions: []iBackend.InternalChefAction{
				{
					EntityType: "policyfile",
					Task:       "update",
					Projects:   []string{"project9"},
				},
				{
					EntityType: "cookbook",
					Task:       "delete",
					Projects:   []string{"project3"},
				},
				{
					EntityType: "node",
					Task:       "create",
					Projects:   []string{},
				},
			},
			expected: map[string]int{
				"policyfile": 1,
				"node":       1,
				"cookbook":   1,
			},
		},
		{
			description: "Action has no projects; request all projects allowed",
			ctx:         contextWithProjects([]string{authzConstants.AllProjectsExternalID}),
			actions: []iBackend.InternalChefAction{
				{
					EntityType: "policyfile",
					Task:       "update",
					Projects:   []string{},
				},
			},
			expected: map[string]int{
				"policyfile": 1,
			},
		},
		{
			description: "Action has no projects; request unassigned projects allowed",
			ctx:         contextWithProjects([]string{authzConstants.UnassignedProjectID}),
			actions: []iBackend.InternalChefAction{
				{
					EntityType: "policyfile",
					Task:       "update",
					Projects:   []string{},
				},
			},
			expected: map[string]int{
				"policyfile": 1,
			},
		},
		{
			description: "Action has a project; request only unassigned projects",
			ctx:         contextWithProjects([]string{authzConstants.UnassignedProjectID}),
			actions: []iBackend.InternalChefAction{
				{
					EntityType: "policyfile",
					Task:       "update",
					Projects:   []string{"project9"},
				},
			},
			expected: map[string]int{},
		},
		{
			description: "Two Actions have and don't have projects; request only unassigned projects",
			ctx:         contextWithProjects([]string{authzConstants.UnassignedProjectID}),
			actions: []iBackend.InternalChefAction{
				{
					EntityType: "policyfile",
					Task:       "update",
					Projects:   []string{"project9"},
				},
				{
					EntityType: "cookbook",
					Task:       "delete",
					Projects:   []string{},
				},
			},
			expected: map[string]int{
				"cookbook": 1,
			},
		},
		{
			description: "Action has a project; request unassigned and matching project allowed",
			ctx:         contextWithProjects([]string{authzConstants.UnassignedProjectID, "project9"}),
			actions: []iBackend.InternalChefAction{
				{
					EntityType: "policyfile",
					Task:       "update",
					Projects:   []string{"project9"},
				},
			},
			expected: map[string]int{
				"policyfile": 1,
			},
		},
		{
			description: "Action has no projects; request has no projects",
			ctx:         contextWithProjects([]string{}),
			actions: []iBackend.InternalChefAction{
				{
					EntityType: "policyfile",
					Task:       "update",
					Projects:   []string{},
				},
			},
			expected: map[string]int{
				"policyfile": 1,
			},
		},
		{
			description: "Two Actions have and don't have projects; request has no projects",
			ctx:         contextWithProjects([]string{}),
			actions: []iBackend.InternalChefAction{
				{
					EntityType: "policyfile",
					Task:       "update",
					Projects:   []string{},
				},
				{
					EntityType: "cookbook",
					Task:       "delete",
					Projects:   []string{"project9"},
				},
			},
			expected: map[string]int{
				"policyfile": 1,
				"cookbook":   1,
			},
		},
		{
			description: "Action with one project matching one of several requested projects allowed",
			ctx:         contextWithProjects([]string{"project3", "project9", "project7", "project6"}),
			actions: []iBackend.InternalChefAction{
				{
					EntityType: "policyfile",
					Task:       "update",
					Projects:   []string{"project9"},
				},
			},
			expected: map[string]int{
				"policyfile": 1,
			},
		},
		{
			description: "Two actions with one project matching different one of several requested projects allowed",
			ctx:         contextWithProjects([]string{"project3", "project9", "project7", "project6"}),
			actions: []iBackend.InternalChefAction{
				{
					EntityType: "policyfile",
					Task:       "update",
					Projects:   []string{"project9"},
				},
				{
					EntityType: "cookbook",
					Task:       "delete",
					Projects:   []string{"project3"},
				},
			},
			expected: map[string]int{
				"policyfile": 1,
				"cookbook":   1,
			},
		},
		{
			description: "Action with one project not matching any of several requested projects allowed",
			ctx:         contextWithProjects([]string{"project3", "project4", "project7", "project6"}),
			actions: []iBackend.InternalChefAction{
				{
					EntityType: "policyfile",
					Task:       "update",
					Projects:   []string{"project9"},
				},
			},
			expected: map[string]int{},
		},
		{
			description: "Two Actions with one project not matching any of several requested projects allowed",
			ctx:         contextWithProjects([]string{"project3", "project4", "project7", "project6"}),
			actions: []iBackend.InternalChefAction{
				{
					EntityType: "policyfile",
					Task:       "update",
					Projects:   []string{"project9"},
				},
				{
					EntityType: "cookbook",
					Task:       "delete",
					Projects:   []string{"project10"},
				},
			},
			expected: map[string]int{},
		},
		{
			description: "Action with several projects where one matches a single requested project allowed",
			ctx:         contextWithProjects([]string{"project3"}),
			actions: []iBackend.InternalChefAction{
				{
					EntityType: "policyfile",
					Task:       "update",
					Projects:   []string{"project3", "project4", "project7", "project6"},
				},
			},
			expected: map[string]int{
				"policyfile": 1,
			},
		},
		{
			description: "Two Actions with several projects where one matches a single requested project allowed",
			ctx:         contextWithProjects([]string{"project3"}),
			actions: []iBackend.InternalChefAction{
				{
					EntityType: "policyfile",
					Task:       "update",
					Projects:   []string{"project3", "project4", "project7", "project6"},
				},
				{
					EntityType: "cookbook",
					Task:       "delete",
					Projects:   []string{"project12", "project10", "project11", "project3"},
				},
			},
			expected: map[string]int{
				"policyfile": 1,
				"cookbook":   1,
			},
		},
		{
			description: "Two Actions with several projects where only one action's project matches a single requested project allowed",
			ctx:         contextWithProjects([]string{"project3"}),
			actions: []iBackend.InternalChefAction{
				{
					EntityType: "policyfile",
					Task:       "update",
					Projects:   []string{"project3", "project4", "project7", "project6"},
				},
				{
					EntityType: "cookbook",
					Task:       "delete",
					Projects:   []string{"project12", "project10", "project11", "project13"},
				},
			},
			expected: map[string]int{
				"policyfile": 1,
			},
		},
		{
			description: "Action with several projects where one matches one of several requested project allowed",
			ctx:         contextWithProjects([]string{"project3", "project10", "project12", "project13"}),
			actions: []iBackend.InternalChefAction{
				{
					EntityType: "policyfile",
					Task:       "update",
					Projects:   []string{"project3", "project4", "project7", "project6"},
				},
			},
			expected: map[string]int{
				"policyfile": 1,
			},
		},
		{
			description: "Two Actions with several projects where one matches one of several requested project allowed",
			ctx:         contextWithProjects([]string{"project3", "project10", "project12", "project13"}),
			actions: []iBackend.InternalChefAction{
				{
					EntityType: "policyfile",
					Task:       "update",
					Projects:   []string{"project3", "project4", "project7", "project6"},
				},
				{
					EntityType: "cookbook",
					Task:       "delete",
					Projects:   []string{"project13", "project14", "project17", "project16"},
				},
			},
			expected: map[string]int{
				"policyfile": 1,
				"cookbook":   1,
			},
		},
		{
			description: "Action with several projects where none matches several requested project allowed",
			ctx:         contextWithProjects([]string{"project14", "project10", "project12", "project13"}),
			actions: []iBackend.InternalChefAction{
				{
					EntityType: "policyfile",
					Task:       "update",
					Projects:   []string{"project3", "project4", "project7", "project6"},
				},
			},
			expected: map[string]int{},
		},
		{
			description: "Action with several projects where two matches two of several requested project allowed",
			ctx:         contextWithProjects([]string{"project3", "project10", "project12", "project13"}),
			actions: []iBackend.InternalChefAction{
				{
					EntityType: "policyfile",
					Task:       "update",
					Projects:   []string{"project3", "project10", "project7", "project6"},
				},
			},
			expected: map[string]int{
				"policyfile": 1,
			},
		},
	}

	for _, test := range cases {
		t.Run(fmt.Sprintf("Project filter: %s", test.description), func(t *testing.T) {
			for index := range test.actions {
				test.actions[index].RecordedAt = time.Now()
				test.actions[index].Id = newUUID()
				// test.actions[index].Task = task
			}

			suite.IngestActions(test.actions)
			defer suite.DeleteAllDocuments()

			res, err := cfgmgmt.GetEventStringBuckets(test.ctx, &request)
			require.NoError(t, err)

			itemsCount := map[string]int{}
			// test response
			for _, eventString := range res.Strings {

				for _, item := range eventString.Collection {
					if len(item.EventsCount) > 0 {
						itemsCount[item.EventsCount[0].Name]++
					}
				}
			}
			for key, expectedValue := range test.expected {
				value, ok := itemsCount[key]

				assert.True(t, ok, "Event type '%s' was not found on string", key)
				assert.Equal(t, expectedValue, value,
					"Number of '%s' on string was %v should be %v", key, value, expectedValue)
			}

			for key := range itemsCount {
				if key != "" {
					_, ok := test.expected[key]
					assert.True(t, ok, "Event type '%s' should not be on the string", key)
				}
			}
		})
	}
}

func TestEventStringsFilterEventType(t *testing.T) {
	var (
		ctx          = context.Background()
		totalActions = 24
		timezone     = "America/Los_Angeles"
		startDate    = time.Now().AddDate(0, 0, -3)
		endDate      = time.Now()
		task         = "update"
		actions      = []iBackend.InternalChefAction{}
		eventTypes   = []string{"policyfile", "node", "cookbook", "bag", "environment", "role"}
	)

	for i := 0; i < totalActions; i++ {
		var (
			id        = newUUID()
			time      = endDate.Add(time.Duration(i * int(time.Hour) * -2))
			name      = "action_" + strconv.Itoa(i)
			eventType = eventTypes[i%len(eventTypes)]
		)

		data := iBackend.InternalChefAction{
			Id:         id,
			RecordedAt: time,
			EntityName: name,
			EntityType: eventType,
			Task:       task,
		}
		actions = append(actions, data)
	}

	suite.IngestActions(actions)
	defer suite.DeleteAllDocuments()
	cases := []struct {
		description string
		request     request.EventStrings
		expected    map[string]int
	}{
		{
			description: "should contain all 24 item on update string",
			request: request.EventStrings{
				HoursBetween: 1,
				Start:        startDate.Format("2006-01-02"),
				End:          endDate.Format("2006-01-02"),
				Timezone:     timezone,
			},
			expected: map[string]int{
				"policyfile":  4,
				"node":        4,
				"cookbook":    4,
				"bag":         4,
				"environment": 4,
				"role":        4,
			},
		},
		{
			description: "should return only the 4 policyfile items on the string",
			request: request.EventStrings{
				Filter:       []string{"event-type:policyfile"},
				HoursBetween: 1,
				Start:        startDate.Format("2006-01-02"),
				End:          endDate.Format("2006-01-02"),
				Timezone:     timezone,
			},
			expected: map[string]int{
				"policyfile": 4,
			},
		},
		{
			description: "should return only the 4 node items on the string",
			request: request.EventStrings{
				Filter:       []string{"event-type:node"},
				HoursBetween: 1,
				Start:        startDate.Format("2006-01-02"),
				End:          endDate.Format("2006-01-02"),
				Timezone:     timezone,
			},
			expected: map[string]int{
				"node": 4,
			},
		},
		{
			description: "should return only the 4 cookbook items on the string",
			request: request.EventStrings{
				Filter:       []string{"event-type:cookbook"},
				HoursBetween: 1,
				Start:        startDate.Format("2006-01-02"),
				End:          endDate.Format("2006-01-02"),
				Timezone:     timezone,
			},
			expected: map[string]int{
				"cookbook": 4,
			},
		},
		{
			description: "should return only the 4 bag items on the string",
			request: request.EventStrings{
				Filter:       []string{"event-type:bag"},
				HoursBetween: 1,
				Start:        startDate.Format("2006-01-02"),
				End:          endDate.Format("2006-01-02"),
				Timezone:     timezone,
			},
			expected: map[string]int{
				"bag": 4,
			},
		},
		{
			description: "should return only the 4 environment items on the string",
			request: request.EventStrings{
				Filter:       []string{"event-type:environment"},
				HoursBetween: 1,
				Start:        startDate.Format("2006-01-02"),
				End:          endDate.Format("2006-01-02"),
				Timezone:     timezone,
			},
			expected: map[string]int{
				"environment": 4,
			},
		},
		{
			description: "should return only the 4 role items on the string",
			request: request.EventStrings{
				Filter:       []string{"event-type:role"},
				HoursBetween: 1,
				Start:        startDate.Format("2006-01-02"),
				End:          endDate.Format("2006-01-02"),
				Timezone:     timezone,
			},
			expected: map[string]int{
				"role": 4,
			},
		},
		{
			description: "should return no items on the string",
			request: request.EventStrings{
				Filter:       []string{"event-type:fake"},
				HoursBetween: 1,
				Start:        startDate.Format("2006-01-02"),
				End:          endDate.Format("2006-01-02"),
				Timezone:     timezone,
			},
			expected: map[string]int{},
		},
	}

	// Run all the cases!
	for _, test := range cases {
		t.Run(fmt.Sprintf("with request '%v' it %s", test.request, test.description),
			func(t *testing.T) {
				res, err := cfgmgmt.GetEventStringBuckets(ctx, &test.request)
				if assert.Nil(t, err) {
					for _, eventString := range res.Strings {
						if eventString.EventAction == task {

							itemsCount := map[string]int{}
							for _, item := range eventString.Collection {
								if len(item.EventsCount) > 0 {
									itemsCount[item.EventsCount[0].Name]++
								}
							}

							for key, expectedValue := range test.expected {
								value, ok := itemsCount[key]

								assert.True(t, ok, "Event type '%s' was not found on string", key)
								assert.Equal(t, expectedValue, value,
									"Number of '%s' on string was %v should be %v", key, value, expectedValue)
							}

							for key := range itemsCount {
								if key != "" {
									_, ok := test.expected[key]
									assert.True(t, ok, "Event type '%s' should not be on the string", key)
								}
							}
						}
					}
				}
			})
	}
}

func testEventStringsWithDate(t *testing.T, date time.Time, message string) {
	t.Run(fmt.Sprintf("testEventStringsNormalRequest on the %s", message),
		func(t *testing.T) {
			testEventStringsNormalRequest(t, date)
		})
	t.Run(fmt.Sprintf("testEventStringsDayZoneActionsRequest on the %s", message),
		func(t *testing.T) {
			testEventStringsDayZoneActionsRequest(t, date)
		})
	t.Run(fmt.Sprintf("testEventStringsDayRequest on the %s", message),
		func(t *testing.T) {
			testEventStringsDayRequest(t, date)
		})
	t.Run(fmt.Sprintf("testEventStringsVaringBucketsSizeRequest on the %s", message),
		func(t *testing.T) {
			testEventStringsVaringBucketsSizeRequest(t, date)
		})
	t.Run(fmt.Sprintf("testEventStringsVaringBucketsSizeNoActionsRequest on the %s", message),
		func(t *testing.T) {
			testEventStringsVaringBucketsSizeNoActionsRequest(t, date)
		})
	t.Run(fmt.Sprintf("testEventStringsThreeDayThreeActionsRequest on the %s", message),
		func(t *testing.T) {
			testEventStringsThreeDayThreeActionsRequest(t, date)
		})
	t.Run(fmt.Sprintf("testEventStringsThreeDayThreeActionsInNewYorkRequest on the %s", message),
		func(t *testing.T) {
			testEventStringsThreeDayThreeActionsInNewYorkRequest(t, date)
		})
	t.Run(fmt.Sprintf("testEventStringsDayOneActionsOutsideOfRequest on the %s", message),
		func(t *testing.T) {
			testEventStringsDayOneActionsOutsideOfRequest(t, date)
		})
	t.Run(fmt.Sprintf("testEventStringsMultipleEventTypesAndCounts on the %s", message),
		func(t *testing.T) {
			testEventStringsMultipleEventTypesAndCounts(t, date)
		})
}

func testEventStringsNormalRequest(t *testing.T, date time.Time) {
	var (
		ctx             = context.Background()
		numberOfBuckets = 7 * 8
		totalActions    = numberOfBuckets - 8
		timezone        = "America/Los_Angeles"
		loc, _          = time.LoadLocation(timezone)
		startDate       = date.In(loc)
		timeDiff        = int(time.Hour) * -3
		actions         = createActions(startDate, totalActions, timeDiff)
	)

	suite.IngestActions(actions)
	defer suite.DeleteAllDocuments()

	request := &request.EventStrings{
		HoursBetween: 3,
		Start:        startDate.AddDate(0, 0, -6).Format("2006-01-02"),
		End:          startDate.Format("2006-01-02"),
		Timezone:     timezone,
	}

	eventStrings, err := cfgmgmt.GetEventStringBuckets(ctx, request)

	assert.Nil(t, err)

	printStrings(eventStrings, request)

	assert.Equal(t, 3, len(eventStrings.Strings))

	totalItems := 0
	for _, eventString := range eventStrings.Strings {
		assert.Equal(t, numberOfBuckets, len(eventString.Collection))
		totalItems = totalItems + countItems(eventString)
	}

	assert.Equal(t, totalActions, totalItems)
	assert.Equal(t, request.Start, eventStrings.Start)
	assert.Equal(t, request.End, eventStrings.End)
}

func testEventStringsDayZoneActionsRequest(t *testing.T, date time.Time) {
	var (
		ctx             = context.Background()
		numberOfBuckets = 24
		timezone        = "America/Los_Angeles"
		loc, _          = time.LoadLocation(timezone)
		startDate       = date.In(loc)
		totalActions    = 0
	)

	request := &request.EventStrings{
		HoursBetween: 1,
		Start:        startDate.Format("2006-01-02"),
		End:          startDate.Format("2006-01-02"),
		Timezone:     timezone,
	}

	eventStrings, err := cfgmgmt.GetEventStringBuckets(ctx, request)

	assert.Nil(t, err)

	totalItems := 0
	for _, eventString := range eventStrings.Strings {
		assert.Equal(t, numberOfBuckets, len(eventString.Collection))
		totalItems = totalItems + countItems(eventString)
	}

	assert.Equal(t, totalActions, totalItems)
	assert.Equal(t, request.Start, eventStrings.Start)
	assert.Equal(t, request.End, eventStrings.End)

	printStrings(eventStrings, request)
}

func testEventStringsVaringBucketsSizeRequest(t *testing.T, date time.Time) {
	var (
		ctx          = context.Background()
		timezone     = "America/Los_Angeles"
		loc, _       = time.LoadLocation(timezone)
		endDate      = date.In(loc)
		totalActions = 3
		timeDiff     = int(time.Hour) * -24
	)

	endDate = time.Date(endDate.Year(), endDate.Month(), endDate.Day(),
		12, 30, 0, 0, endDate.Location())

	actions := createActions(endDate, totalActions, timeDiff)

	suite.IngestActions(actions)
	defer suite.DeleteAllDocuments()

	for _, bucketSize := range []int{1, 2, 3, 4, 6, 8, 12, 24} {
		t.Run(fmt.Sprintf("bucketSize %d", bucketSize),
			func(t *testing.T) {
				numberOfBuckets := int(math.Ceil(24.0 * 3.0 / float64(bucketSize)))

				request := &request.EventStrings{
					HoursBetween: int32(bucketSize),
					Start:        endDate.AddDate(0, 0, -2).Format("2006-01-02"),
					End:          endDate.Format("2006-01-02"),
					Timezone:     timezone,
				}

				eventStrings, err := cfgmgmt.GetEventStringBuckets(ctx, request)

				assert.Nil(t, err)
				totalItems := 0
				for _, eventString := range eventStrings.Strings {
					assert.Equal(t, numberOfBuckets, len(eventString.Collection), "with bucket size: %d", bucketSize)
					totalItems = totalItems + countItems(eventString)
				}

				assert.Equal(t, totalActions, totalItems)
				assert.Equal(t, request.Start, eventStrings.Start)
				assert.Equal(t, request.End, eventStrings.End)

				printStrings(eventStrings, request)
			})
	}
}

func testEventStringsVaringBucketsSizeNoActionsRequest(t *testing.T, date time.Time) {
	var (
		ctx          = context.Background()
		timezone     = "America/Los_Angeles"
		loc, _       = time.LoadLocation(timezone)
		endDate      = date.In(loc)
		totalActions = 0
	)

	endDate = time.Date(endDate.Year(), endDate.Month(), endDate.Day(),
		12, 30, 0, 0, endDate.Location())

	for _, bucketSize := range []int{1, 2, 3, 4, 6, 8, 12, 24} {
		t.Run(fmt.Sprintf("bucketSize %d", bucketSize),
			func(t *testing.T) {
				numberOfBuckets := int(math.Ceil(24.0 * 3.0 / float64(bucketSize)))

				request := &request.EventStrings{
					HoursBetween: int32(bucketSize),
					Start:        endDate.AddDate(0, 0, -2).Format("2006-01-02"),
					End:          endDate.Format("2006-01-02"),
					Timezone:     timezone,
				}

				eventStrings, err := cfgmgmt.GetEventStringBuckets(ctx, request)

				assert.Nil(t, err)
				totalItems := 0
				for _, eventString := range eventStrings.Strings {
					assert.Equal(t, numberOfBuckets, len(eventString.Collection), "with bucket size: %d", bucketSize)
					totalItems = totalItems + countItems(eventString)
				}

				assert.Equal(t, totalActions, totalItems)
				assert.Equal(t, request.Start, eventStrings.Start)
				assert.Equal(t, request.End, eventStrings.End)

				printStrings(eventStrings, request)
			})
	}
}

func testEventStringsThreeDayThreeActionsRequest(t *testing.T, date time.Time) {
	var (
		ctx               = context.Background()
		bucketSizeInHours = 1
		numberOfBuckets   = (24 / bucketSizeInHours) * 3
		timezone          = "America/Los_Angeles"
		loc, _            = time.LoadLocation(timezone)
		endDate           = date.In(loc)
		totalActions      = 3
	)
	endDate = time.Date(endDate.Year(), endDate.Month(), endDate.Day(),
		12, 30, 0, 0, endDate.Location())

	action1Time := endDate.AddDate(0, 0, -2)
	action2Time := endDate.AddDate(0, 0, -1)
	action3Time := endDate

	actions := []iBackend.InternalChefAction{
		{
			Id:         newUUID(),
			RecordedAt: action3Time,
			EntityName: "action_3",
			EntityType: "item",
			Task:       "delete",
		},
		{
			Id:         newUUID(),
			RecordedAt: action2Time,
			EntityName: "action_2",
			EntityType: "cookbook",
			Task:       "update",
		},
		{
			Id:         newUUID(),
			RecordedAt: action1Time,
			EntityName: "action_1",
			EntityType: "bag",
			Task:       "create",
		},
	}

	suite.IngestActions(actions)
	defer suite.DeleteAllDocuments()

	startTime := endDate.AddDate(0, 0, -2)
	startTime = time.Date(startTime.Year(), startTime.Month(), startTime.Day(),
		0, 0, 0, 0, startTime.Location())
	request := &request.EventStrings{
		HoursBetween: int32(bucketSizeInHours),
		Start:        startTime.Format("2006-01-02"),
		End:          endDate.Format("2006-01-02"),
		Timezone:     timezone,
	}

	eventStrings, err := cfgmgmt.GetEventStringBuckets(ctx, request)

	assert.Nil(t, err)

	totalItems := 0

	for _, eventString := range eventStrings.Strings {
		assert.Equal(t, numberOfBuckets, len(eventString.Collection))
		totalItems = totalItems + countItems(eventString)
		if eventString.EventAction == "create" {
			for index, item := range eventString.Collection {
				currentBucketStart, currentBucketEnd := getBucketWindow(index, startTime, bucketSizeInHours)
				if withinTimeWindow(currentBucketStart, currentBucketEnd, action1Time) {
					assert.Equal(t, 1, len(item.EventsCount),
						"'create' 'bag' was not found in the %v to %v time window", currentBucketStart, currentBucketEnd)
					if len(item.EventsCount) == 1 {
						assert.Equal(t, "bag", item.EventsCount[0].Name)
					}
				} else {
					assert.Equal(t, 0, len(item.EventsCount),
						"time window %v to %v had the %v values. They should be at %v",
						currentBucketStart, currentBucketEnd, item.EventsCount, action1Time)
				}
			}
		}
		if eventString.EventAction == "update" {
			for index, item := range eventString.Collection {
				currentBucketStart, currentBucketEnd := getBucketWindow(index, startTime, bucketSizeInHours)
				if withinTimeWindow(currentBucketStart, currentBucketEnd, action2Time) {
					assert.Equal(t, 1, len(item.EventsCount),
						"'update' 'cookbook' was not found in the %v to %v time window", currentBucketStart, currentBucketEnd)
					if len(item.EventsCount) == 1 {
						assert.Equal(t, "cookbook", item.EventsCount[0].Name)
					}
				} else {
					assert.Equal(t, 0, len(item.EventsCount),
						"time window %v to %v had the %v values. They should be at %v",
						currentBucketStart, currentBucketEnd, item.EventsCount, action2Time)
				}
			}
		}
		if eventString.EventAction == "delete" {
			for index, item := range eventString.Collection {
				currentBucketStart, currentBucketEnd := getBucketWindow(index, startTime, bucketSizeInHours)
				if withinTimeWindow(currentBucketStart, currentBucketEnd, action3Time) {
					assert.Equal(t, 1, len(item.EventsCount),
						"'delete' 'item' was not found in the %v to %v time window", currentBucketStart, currentBucketEnd)
					if len(item.EventsCount) == 1 {
						assert.Equal(t, "item", item.EventsCount[0].Name)
					}
				} else {
					assert.Equal(t, 0, len(item.EventsCount),
						"time window %v to %v had the %v values. They should be at %v",
						currentBucketStart, currentBucketEnd, item.EventsCount, action2Time)
				}
			}
		}
	}

	assert.Equal(t, totalActions, totalItems)
	assert.Equal(t, request.Start, eventStrings.Start)
	assert.Equal(t, request.End, eventStrings.End)

	printStrings(eventStrings, request)
}

func testEventStringsThreeDayThreeActionsInNewYorkRequest(t *testing.T, date time.Time) {
	var (
		ctx               = context.Background()
		bucketSizeInHours = 1
		numberOfBuckets   = (24 / bucketSizeInHours) * 3
		timezone          = "America/New_York"
		loc, _            = time.LoadLocation(timezone)
		endDate           = date.In(loc)
		totalActions      = 3
	)
	endDate = time.Date(endDate.Year(), endDate.Month(), endDate.Day(),
		17, 30, 0, 0, endDate.Location())

	action1Time := endDate.AddDate(0, 0, -2)
	action2Time := endDate.AddDate(0, 0, -1)
	action3Time := endDate

	actions := []iBackend.InternalChefAction{
		{
			Id:         newUUID(),
			RecordedAt: action3Time,
			EntityName: "action_3",
			EntityType: "item",
			Task:       "delete",
		},
		{
			Id:         newUUID(),
			RecordedAt: action2Time,
			EntityName: "action_2",
			EntityType: "cookbook",
			Task:       "update",
		},
		{
			Id:         newUUID(),
			RecordedAt: action1Time,
			EntityName: "action_1",
			EntityType: "bag",
			Task:       "create",
		},
	}

	suite.IngestActions(actions)
	defer suite.DeleteAllDocuments()
	startTime := endDate.AddDate(0, 0, -2)
	startTime = time.Date(startTime.Year(), startTime.Month(), startTime.Day(),
		0, 0, 0, 0, startTime.Location())
	request := &request.EventStrings{
		HoursBetween: int32(bucketSizeInHours),
		Start:        startTime.Format("2006-01-02"),
		End:          endDate.Format("2006-01-02"),
		Timezone:     timezone,
	}

	eventStrings, err := cfgmgmt.GetEventStringBuckets(ctx, request)

	assert.Nil(t, err)

	totalItems := 0
	for _, eventString := range eventStrings.Strings {
		assert.Equal(t, numberOfBuckets, len(eventString.Collection))
		totalItems = totalItems + countItems(eventString)
		if eventString.EventAction == "create" {
			for index, item := range eventString.Collection {
				currentBucketStart, currentBucketEnd := getBucketWindow(index, startTime, bucketSizeInHours)
				if withinTimeWindow(currentBucketStart, currentBucketEnd, action1Time) {
					assert.Equal(t, 1, len(item.EventsCount), "a 'create bag' event should be in time window %v to %v", currentBucketStart, currentBucketEnd)
					if len(item.EventsCount) == 1 {
						assert.Equal(t, "bag", item.EventsCount[0].Name)
					}
				} else {
					assert.Equal(t, 0, len(item.EventsCount),
						"create event found in time window %v to %v with values %v", currentBucketStart, currentBucketEnd, item.EventsCount)
				}
			}
		}
		if eventString.EventAction == "update" {
			for index, item := range eventString.Collection {
				currentBucketStart, currentBucketEnd := getBucketWindow(index, startTime, bucketSizeInHours)
				if withinTimeWindow(currentBucketStart, currentBucketEnd, action2Time) {
					assert.Equal(t, 1, len(item.EventsCount), "a 'update cookbook' event should be at index %d", index)
					if len(item.EventsCount) == 1 {
						assert.Equal(t, "cookbook", item.EventsCount[0].Name)
					}
				} else {
					assert.Equal(t, 0, len(item.EventsCount), "update event found at index %d values: %v", index, item.EventsCount)
				}
			}
		}
		if eventString.EventAction == "delete" {
			for index, item := range eventString.Collection {
				currentBucketStart, currentBucketEnd := getBucketWindow(index, startTime, bucketSizeInHours)
				if withinTimeWindow(currentBucketStart, currentBucketEnd, action3Time) {
					assert.Equal(t, 1, len(item.EventsCount), "a 'delete item' event should be at index %d", index)
					if len(item.EventsCount) == 1 {
						assert.Equal(t, "item", item.EventsCount[0].Name)
					}
				} else {
					assert.Equal(t, 0, len(item.EventsCount), "delete event found at index %d values: %v", index, item.EventsCount)
				}
			}
		}
	}

	assert.Equal(t, totalActions, totalItems)
	assert.Equal(t, request.Start, eventStrings.Start)
	assert.Equal(t, request.End, eventStrings.End)

	printStrings(eventStrings, request)
}

func testEventStringsDayOneActionsOutsideOfRequest(t *testing.T, date time.Time) {
	var (
		ctx                 = context.Background()
		numberOfBuckets     = 24
		timezone            = "America/Los_Angeles"
		loc, _              = time.LoadLocation(timezone)
		startDate           = date.In(loc)
		totalInRangeActions = 0
		timeDiff            = int(time.Hour) * -1
	)
	startDate = time.Date(startDate.Year(), startDate.Month(), startDate.Day(),
		12, 0, 0, 0, startDate.Location())

	actions := createActions(startDate, 1, timeDiff)

	suite.IngestActions(actions)
	defer suite.DeleteAllDocuments()

	request := &request.EventStrings{
		HoursBetween: 1,
		Start:        startDate.AddDate(0, 0, -1).Format("2006-01-02"),
		End:          startDate.AddDate(0, 0, -1).Format("2006-01-02"),
		Timezone:     timezone,
	}

	eventStrings, err := cfgmgmt.GetEventStringBuckets(ctx, request)

	assert.Nil(t, err)

	totalItems := 0
	for _, eventString := range eventStrings.Strings {
		assert.Equal(t, numberOfBuckets, len(eventString.Collection))
		totalItems = totalItems + countItems(eventString)
	}

	assert.Equal(t, totalInRangeActions, totalItems)
	assert.Equal(t, request.Start, eventStrings.Start)
	assert.Equal(t, request.End, eventStrings.End)

	printStrings(eventStrings, request)
}

func testEventStringsDayRequest(t *testing.T, date time.Time) {
	var (
		ctx             = context.Background()
		numberOfBuckets = 24
		timezone        = "America/Los_Angeles"
		loc, _          = time.LoadLocation(timezone)
		startDate       = date.In(loc)
		totalActions    = 24
		timeDiff        = int(time.Hour) * -1
	)
	startDate = time.Date(startDate.Year(), startDate.Month(), startDate.Day(),
		23, 59, 59, 0, startDate.Location())

	actions := createActions(startDate, totalActions, timeDiff)

	suite.IngestActions(actions)
	defer suite.DeleteAllDocuments()

	request := &request.EventStrings{
		HoursBetween: 1,
		Start:        startDate.Format("2006-01-02"),
		End:          startDate.Format("2006-01-02"),
		Timezone:     timezone,
	}

	eventStrings, err := cfgmgmt.GetEventStringBuckets(ctx, request)

	assert.Nil(t, err)

	totalItems := 0
	for _, eventString := range eventStrings.Strings {
		assert.Equal(t, numberOfBuckets, len(eventString.Collection))
		totalItems = totalItems + countItems(eventString)
	}

	assert.Equal(t, totalActions, totalItems)
	assert.Equal(t, request.Start, eventStrings.Start)
	assert.Equal(t, request.End, eventStrings.End)

	printStrings(eventStrings, request)
}

func testEventStringsMultipleEventTypesAndCounts(t *testing.T, date time.Time) {
	var (
		ctx               = context.Background()
		bucketSizeInHours = 1
		numberOfBuckets   = (24 / bucketSizeInHours) * 3
		timezone          = "America/Los_Angeles"
		loc, _            = time.LoadLocation(timezone)
		endDate           = date.In(loc)
		totalActions      = 3
	)
	endDate = time.Date(endDate.Year(), endDate.Month(), endDate.Day(),
		12, 30, 0, 0, endDate.Location())

	startTime := endDate.AddDate(0, 0, -2)
	startTime = time.Date(startTime.Year(), startTime.Month(), startTime.Day(),
		0, 0, 0, 0, startTime.Location())

	action1Time := endDate.AddDate(0, 0, -2)
	action2Time := endDate.AddDate(0, 0, -1)
	action3Time := endDate

	actions := []iBackend.InternalChefAction{
		// Delete - Five different Events
		{
			Id: newUUID(), RecordedAt: action3Time, EntityName: "mock_action",
			EntityType: "item1", Task: "delete"},
		{
			Id: newUUID(), RecordedAt: action3Time, EntityName: "mock_action",
			EntityType: "item2", Task: "delete"},
		{
			Id: newUUID(), RecordedAt: action3Time, EntityName: "mock_action",
			EntityType: "item3", Task: "delete"},
		{
			Id: newUUID(), RecordedAt: action3Time, EntityName: "mock_action",
			EntityType: "item4", Task: "delete"},
		{
			Id: newUUID(), RecordedAt: action3Time, EntityName: "mock_action",
			EntityType: "item5", Task: "delete"},
		// Update - Five Events same type (Count)
		{
			Id: newUUID(), RecordedAt: action2Time, EntityName: "mock_action",
			EntityType: "cookbook", Task: "update"},
		{
			Id: newUUID(), RecordedAt: action2Time, EntityName: "mock_action",
			EntityType: "cookbook", Task: "update"},
		{
			Id: newUUID(), RecordedAt: action2Time, EntityName: "mock_action",
			EntityType: "cookbook", Task: "update"},
		{
			Id: newUUID(), RecordedAt: action2Time, EntityName: "mock_action",
			EntityType: "cookbook", Task: "update"},
		{
			Id: newUUID(), RecordedAt: action2Time, EntityName: "mock_action",
			EntityType: "cookbook", Task: "update"},
		// Create - (Mix) Two different Events twice each
		{
			Id: newUUID(), RecordedAt: action1Time, EntityName: "mock_action",
			EntityType: "bag", Task: "create"},
		{
			Id: newUUID(), RecordedAt: action1Time, EntityName: "mock_action",
			EntityType: "bag", Task: "create"},
		{
			Id: newUUID(), RecordedAt: action1Time, EntityName: "mock_action",
			EntityType: "role", Task: "create"},
		{
			Id: newUUID(), RecordedAt: action1Time, EntityName: "mock_action",
			EntityType: "role", Task: "create"},
	}

	suite.IngestActions(actions)
	defer suite.DeleteAllDocuments()

	request := &request.EventStrings{
		HoursBetween: int32(bucketSizeInHours),
		Start:        startTime.Format("2006-01-02"),
		End:          endDate.Format("2006-01-02"),
		Timezone:     timezone,
	}

	eventStrings, err := cfgmgmt.GetEventStringBuckets(ctx, request)

	assert.Nil(t, err)

	totalItems := 0
	for _, eventString := range eventStrings.Strings {
		assert.Equal(t, numberOfBuckets, len(eventString.Collection))
		totalItems = totalItems + countItems(eventString)
		if eventString.EventAction == "create" {
			for index, item := range eventString.Collection {
				currentBucketStart, currentBucketEnd := getBucketWindow(index, startTime, bucketSizeInHours)
				if withinTimeWindow(currentBucketStart, currentBucketEnd, action1Time) {
					assert.Equal(t, 2, len(item.EventsCount))
					if len(item.EventsCount) == 2 {
						assert.Equal(t, "bag", item.EventsCount[0].Name)
						assert.Equal(t, int64(2), item.EventsCount[0].Count)
						assert.Equal(t, "role", item.EventsCount[1].Name)
						assert.Equal(t, int64(2), item.EventsCount[1].Count)
					}
				} else {
					assert.Equal(t, 0, len(item.EventsCount))
				}
			}
		}
		if eventString.EventAction == "update" {
			for index, item := range eventString.Collection {
				currentBucketStart, currentBucketEnd := getBucketWindow(index, startTime, bucketSizeInHours)
				if withinTimeWindow(currentBucketStart, currentBucketEnd, action2Time) {
					assert.Equal(t, 1, len(item.EventsCount))
					if len(item.EventsCount) == 1 {
						assert.Equal(t, "cookbook", item.EventsCount[0].Name)
						assert.Equal(t, int64(5), item.EventsCount[0].Count)
					}
				} else {
					assert.Equal(t, 0, len(item.EventsCount))
				}
			}
		}
		if eventString.EventAction == "delete" {
			for index, item := range eventString.Collection {
				currentBucketStart, currentBucketEnd := getBucketWindow(index, startTime, bucketSizeInHours)
				if withinTimeWindow(currentBucketStart, currentBucketEnd, action3Time) {
					assert.Equal(t, 5, len(item.EventsCount))
					for i, event := range item.EventsCount {
						assert.Equal(t, fmt.Sprintf("item%d", i+1), event.Name)
						assert.Equal(t, int64(1), event.Count)
					}
				} else {
					assert.Equal(t, 0, len(item.EventsCount))
				}
			}
		}
	}

	assert.Equal(t, totalActions, totalItems)
	assert.Equal(t, request.Start, eventStrings.Start)
	assert.Equal(t, request.End, eventStrings.End)

	printStrings(eventStrings, request)
}

func getBucketWindow(index int, startTime time.Time, bucketSizeInHours int) (time.Time, time.Time) {
	bucketStart := startTime.Add(time.Hour * time.Duration(index))
	bucketEnd := bucketStart.Add(time.Hour * time.Duration(bucketSizeInHours))

	return bucketStart, bucketEnd
}

func countItems(eventString *response.EventString) int {
	totalItems := 0
	for _, item := range eventString.Collection {
		if len(item.EventsCount) > 0 {
			if item.EventsCount[0].Name != "" {
				totalItems++
			}
		}
	}
	return totalItems
}

func printStrings(eventStrings *response.EventStrings, request *request.EventStrings) {

	counts := make([]int, len(eventStrings.Strings))

	numberOfBucketsPerDay := int(math.Ceil(24.0 / float64(eventStrings.HoursBetween)))
	for index, eventString := range eventStrings.Strings {
		counts[index] = countItems(eventString)
	}

	if len(eventStrings.Strings) <= 0 {
		return
	}

	length := len(eventStrings.Strings[0].Collection)

	for index, eventString := range eventStrings.Strings {
		fmt.Printf("%s = %d ", eventString.EventAction, counts[index])
	}
	fmt.Println()

	loc, err := time.LoadLocation(request.Timezone)
	if err != nil {
		return
	}

	startTime, err := time.ParseInLocation("2006-01-02", eventStrings.Start, loc)
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

	for _, eventString := range eventStrings.Strings {
		for index, eventType := range eventString.Collection {
			if index%numberOfBucketsPerDay == 0 {
				fmt.Print("|")
			}
			if len(eventType.EventsCount) > 0 {
				fmt.Printf("_%s_", string(eventType.EventsCount[0].Name))
			} else {
				fmt.Print("___")
			}
		}
		fmt.Printf("|'%s'\n", eventString.EventAction)
	}

	for index := 0; index < length; index++ {
		fmt.Print("---")
	}
	for index := 0; index <= len(dates); index++ {
		fmt.Print("-")
	}
	fmt.Println()
}

func withinTimeWindow(startTime time.Time, endTime time.Time, testTime time.Time) bool {
	return testTime.Equal(startTime) || (testTime.After(startTime) && testTime.Before(endTime))
}
