package integration_test

import (
	"context"
	"testing"
	"time"

	"github.com/chef/automate/api/interservice/compliance/reporting"
	reportingServer "github.com/chef/automate/components/compliance-service/api/reporting/server"
	"github.com/chef/automate/components/compliance-service/reporting/relaxting"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestNodeControl(t *testing.T) {
	server := reportingServer.New(&relaxting.ES2Backend{ESUrl: elasticsearchUrl})
	ctx := context.Background()
	endTime := time.Now()
	IDs := []string{newUUID(), newUUID(), newUUID(), newUUID(), newUUID()}
	cases := []struct {
		description         string
		reportID            string
		reports             []*relaxting.ESInSpecReport
		query               reporting.Query
		expectedTitles      []string
		expectedResultCount int
	}{
		{
			description: "ListControlInfo: check for paginated response",
			reports: []*relaxting.ESInSpecReport{
				{
					NodeID: "1",
					Profiles: []relaxting.ESInSpecReportProfile{
						{
							Name: "linux1",
							Controls: []relaxting.ESInSpecReportControl{
								{
									Title: "a2-prod",
								},
								{
									Title: "a2-test",
								},
								{
									Title: "a2-dev",
								},
								{
									Title: "a2-canary",
								},
							},
						},
					},
					EndTime: endTime,
				},
			},
			query: reporting.Query{
				Filters: []*reporting.ListFilter{
					{
						Type:   "end_time",
						Values: []string{time.Now().Format("2006-01-02T15:04:05Z")},
					},
					{
						Type:   "from",
						Values: []string{"0"},
					},
					{
						Type:   "size",
						Values: []string{"2"},
					},
				},
			},
			expectedTitles:      []string{"a2-prod", "a2-test"},
			expectedResultCount: 2,
		},
		{
			description: "ListControlInfo: check without paginated query params",
			reports: []*relaxting.ESInSpecReport{
				{
					NodeID: "1",
					Profiles: []relaxting.ESInSpecReportProfile{
						{
							Name: "linux1",
							Controls: []relaxting.ESInSpecReportControl{
								{
									Title: "a2-prod",
								},
								{
									Title: "a2-test",
								},
								{
									Title: "a2-dev",
								},
								{
									Title: "a2-canary",
								},
								{
									Title: "a2-prod1",
								},
								{
									Title: "a2-test1",
								},
								{
									Title: "a2-dev1",
								},
								{
									Title: "a2-canary1",
								},
								{
									Title: "a2-prod3",
								},
								{
									Title: "a2-test2",
								},
								{
									Title: "a2-dev2",
								},
								{
									Title: "a2-canary2",
								},
							},
						},
					},
					EndTime: endTime,
				},
			},
			query: reporting.Query{
				Filters: []*reporting.ListFilter{
					{
						Type:   "end_time",
						Values: []string{time.Now().Format("2006-01-02T15:04:05Z")},
					},
					{
						Type:   "from",
						Values: []string{"0"},
					},
				},
			},
			expectedTitles:      []string{"a2-prod", "a2-test", "a2-dev"},
			expectedResultCount: 10,
		},
		{
			description: "ListControlInfo: check for paginated response from multiple profiles",
			reports: []*relaxting.ESInSpecReport{
				{
					NodeID: "1",
					Profiles: []relaxting.ESInSpecReportProfile{
						{
							Name: "linux1",
							Controls: []relaxting.ESInSpecReportControl{
								{
									Title: "a2-prod",
								},
								{
									Title: "a2-test",
								},
							},
						},
						{
							Name: "linux2",
							Controls: []relaxting.ESInSpecReportControl{
								{
									Title: "a2-prod",
								},
								{
									Title: "a2-test",
								},
							},
						},
					},
					EndTime: endTime,
				},
			},
			query: reporting.Query{
				Filters: []*reporting.ListFilter{
					{
						Type:   "end_time",
						Values: []string{time.Now().Format("2006-01-02T15:04:05Z")},
					},
					{
						Type:   "from",
						Values: []string{"0"},
					},
					{
						Type:   "size",
						Values: []string{"3"},
					},
				},
			},
			expectedTitles:      []string{"a2-prod", "a2-test", "a2-prod"},
			expectedResultCount: 3,
		},
		{
			description: "ListControlInfo: check for paginated response from non zero start",
			reports: []*relaxting.ESInSpecReport{
				{
					NodeID: "1",
					Profiles: []relaxting.ESInSpecReportProfile{
						{
							Name: "linux1",
							Controls: []relaxting.ESInSpecReportControl{
								{
									Title: "a2-prod",
								},
								{
									Title: "a2-test",
								},
							},
						},
						{
							Name: "linux2",
							Controls: []relaxting.ESInSpecReportControl{
								{
									Title: "a2-prod",
								},
								{
									Title: "a2-test",
								},
							},
						},
					},
					EndTime: endTime,
				},
			},
			query: reporting.Query{
				Filters: []*reporting.ListFilter{
					{
						Type:   "end_time",
						Values: []string{time.Now().Format("2006-01-02T15:04:05Z")},
					},
					{
						Type:   "from",
						Values: []string{"1"},
					},
					{
						Type:   "size",
						Values: []string{"3"},
					},
				},
			},
			expectedTitles:      []string{"a2-test", "a2-prod", "a2-test"},
			expectedResultCount: 3,
		},
		{
			description: "ListControlInfo: check for paginated response with fitler",
			reports: []*relaxting.ESInSpecReport{
				{
					NodeID: "1",
					Profiles: []relaxting.ESInSpecReportProfile{
						{
							Name:   "linux1",
							SHA256: IDs[0],
							Controls: []relaxting.ESInSpecReportControl{
								{
									Title: "a2-prod",
								},
							},
						},
						{
							Name:   "linux2",
							SHA256: newUUID(),
							Controls: []relaxting.ESInSpecReportControl{
								{
									Title: "a2-dev",
								},
								{
									Title: "a2-canary",
								},
							},
						},
					},
					EndTime: endTime,
				},
			},
			query: reporting.Query{
				Filters: []*reporting.ListFilter{
					{
						Type:   "end_time",
						Values: []string{time.Now().Format("2006-01-02T15:04:05Z")},
					},
					{
						Type:   "from",
						Values: []string{"0"},
					},
					{
						Type:   "size",
						Values: []string{"2"},
					},
					{
						Type:   "profile_id",
						Values: []string{IDs[0]},
					},
				},
			},
			expectedTitles:      []string{"a2-prod"},
			expectedResultCount: 1,
		},
		{
			description: "ListControlInfo: check for paginated response with control fitler",
			reports: []*relaxting.ESInSpecReport{
				{
					NodeID: "1",
					Profiles: []relaxting.ESInSpecReportProfile{
						{
							Name:   "linux1",
							SHA256: IDs[0],
							Controls: []relaxting.ESInSpecReportControl{
								{
									ID:    "a2-dev1",
									Title: "a2-dev1 for control1",
								},
								{
									ID:    "a2-canary1",
									Title: "a2-canary1 for control1",
								},
							},
						},
						{
							Name:   "linux2",
							SHA256: IDs[1],
							Controls: []relaxting.ESInSpecReportControl{
								{
									ID:    "a2-dev2",
									Title: "a2-dev2 for control2",
								},
								{
									ID:    "a2-canary2",
									Title: "a2-canary2 for control2",
								},
							},
						},
						{
							Name:   "linux3",
							SHA256: IDs[2],
							Controls: []relaxting.ESInSpecReportControl{
								{
									ID:    "a2-dev3",
									Title: "a2-dev3 for control3",
								},
								{
									ID:    "a2-canary3",
									Title: "a2-canary3 for control3",
								},
							},
						},
						{
							Name:   "linux4",
							SHA256: IDs[3],
							Controls: []relaxting.ESInSpecReportControl{
								{
									ID:    "a2-dev4",
									Title: "a2-dev4 for control4",
								},
								{
									ID:    "a2-canary4",
									Title: "a2-canary4 for control4",
								},
							},
						},
					},
					EndTime: endTime,
				},
			},
			query: reporting.Query{
				Filters: []*reporting.ListFilter{
					{
						Type:   "end_time",
						Values: []string{time.Now().Format("2006-01-02T15:04:05Z")},
					},
					{
						Type:   "from",
						Values: []string{"0"},
					},
					{
						Type:   "size",
						Values: []string{"5"},
					},
					{
						Type:   "control",
						Values: []string{"a2-dev4"},
					},
				},
			},
			expectedTitles:      []string{"a2-dev4 for control4"},
			expectedResultCount: 1,
		},
		{
			description: "ListControlInfo: check for paginated response with multiple control fitler",
			reports: []*relaxting.ESInSpecReport{
				{
					NodeID: "1",
					Profiles: []relaxting.ESInSpecReportProfile{
						{
							Name:   "linux1",
							SHA256: IDs[0],
							Controls: []relaxting.ESInSpecReportControl{
								{
									ID:    "a2-dev1",
									Title: "a2-dev1 for control1",
								},
								{
									ID:    "a2-canary1",
									Title: "a2-canary1 for control1",
								},
							},
						},
						{
							Name:   "linux2",
							SHA256: IDs[1],
							Controls: []relaxting.ESInSpecReportControl{
								{
									ID:    "a2-dev2",
									Title: "a2-dev2 for control2",
								},
								{
									ID:    "a2-canary2",
									Title: "a2-canary2 for control2",
								},
							},
						},
						{
							Name:   "linux3",
							SHA256: IDs[2],
							Controls: []relaxting.ESInSpecReportControl{
								{
									ID:    "a2-dev3",
									Title: "a2-dev3 for control3",
								},
								{
									ID:    "a2-canary3",
									Title: "a2-canary3 for control3",
								},
							},
						},
						{
							Name:   "linux4",
							SHA256: IDs[3],
							Controls: []relaxting.ESInSpecReportControl{
								{
									ID:    "a2-dev4",
									Title: "a2-dev4 for control4",
								},
								{
									ID:    "a2-canary4",
									Title: "a2-canary4 for control4",
								},
							},
						},
					},
					EndTime: endTime,
				},
			},
			query: reporting.Query{
				Filters: []*reporting.ListFilter{
					{
						Type:   "end_time",
						Values: []string{time.Now().Format("2006-01-02T15:04:05Z")},
					},
					{
						Type:   "from",
						Values: []string{"0"},
					},
					{
						Type:   "size",
						Values: []string{"5"},
					},
					{
						Type:   "control",
						Values: []string{"a2-dev4", "a2-canary3"},
					},
				},
			},
			expectedTitles:      []string{"a2-canary3 for control3", "a2-dev4 for control4"},
			expectedResultCount: 2,
		},
		{
			description: "ListControlInfo: check for paginated response with project and control fitler",
			reports: []*relaxting.ESInSpecReport{
				{
					NodeID: "1",
					Profiles: []relaxting.ESInSpecReportProfile{
						{
							Name:   "linux1",
							SHA256: IDs[0],
							Controls: []relaxting.ESInSpecReportControl{
								{
									ID:    "a2-dev1",
									Title: "a2-dev1 for control1",
								},
								{
									ID:    "a2-canary1",
									Title: "a2-canary1 for control1",
								},
							},
						},
						{
							Name:   "linux2",
							SHA256: IDs[1],
							Controls: []relaxting.ESInSpecReportControl{
								{
									ID:    "a2-dev2",
									Title: "a2-dev2 for control2",
								},
								{
									ID:    "a2-canary2",
									Title: "a2-canary2 for control2",
								},
							},
						},
						{
							Name:   "linux3",
							SHA256: IDs[2],
							Controls: []relaxting.ESInSpecReportControl{
								{
									ID:    "a2-dev3",
									Title: "a2-dev3 for control3",
								},
								{
									ID:    "a2-canary3",
									Title: "a2-canary3 for control3",
								},
							},
						},
						{
							Name:   "linux4",
							SHA256: IDs[3],
							Controls: []relaxting.ESInSpecReportControl{
								{
									ID:    "a2-dev4",
									Title: "a2-dev4 for control4",
								},
								{
									ID:    "a2-canary4",
									Title: "a2-canary4 for control4",
								},
							},
						},
					},
					EndTime: endTime,
				},
			},
			query: reporting.Query{
				Filters: []*reporting.ListFilter{
					{
						Type:   "end_time",
						Values: []string{time.Now().Format("2006-01-02T15:04:05Z")},
					},
					{
						Type:   "from",
						Values: []string{"0"},
					},
					{
						Type:   "size",
						Values: []string{"5"},
					},
					{
						Type:   "profile_id",
						Values: []string{IDs[3]},
					},
					{
						Type:   "control",
						Values: []string{"a2-dev4", "a2-canary3"},
					},
				},
			},
			expectedTitles:      []string{"a2-dev4 for control4"},
			expectedResultCount: 1,
		},
	}

	for _, test := range cases {
		t.Run(test.description, func(t *testing.T) {
			defer suite.DeleteAllDocuments()
			rIDs, err := suite.InsertInspecReports(test.reports)
			assert.NoError(t, err)
			require.NotNil(t, rIDs)
			test.query.Id = rIDs[0]
			response, err := server.ListControlInfo(ctx, &test.query)
			assert.NoError(t, err)
			require.NotNil(t, response)
			require.Equal(t, test.expectedResultCount, len(response.ControlElements), "expected %d but got %d", test.expectedResultCount, len(response.ControlElements))
			for i := range test.expectedTitles {
				assert.Equal(t, response.ControlElements[i].Title, test.expectedTitles[i], "expected %s but got %s", test.expectedTitles[i], response.ControlElements[i].Title)
			}
		})
	}
}

func TestNodeControlWithError(t *testing.T) {
	server := reportingServer.New(&relaxting.ES2Backend{ESUrl: elasticsearchUrl})
	ctx := context.Background()
	endTime := time.Now()
	IDs := []string{newUUID(), newUUID(), newUUID(), newUUID(), newUUID()}
	cases := []struct {
		description         string
		reportID            string
		reports             []*relaxting.ESInSpecReport
		query               reporting.Query
		expectedTitles      []string
		expectedResultCount int
	}{
		{
			description: "ListControlInfo: error when filtered with non existing profile id",
			reports: []*relaxting.ESInSpecReport{
				{
					NodeID: "1",
					Profiles: []relaxting.ESInSpecReportProfile{
						{
							Name:   "linux1",
							SHA256: IDs[0],
							Controls: []relaxting.ESInSpecReportControl{
								{
									ID:    "a2-dev1",
									Title: "a2-dev1 for control1",
								},
								{
									ID:    "a2-canary1",
									Title: "a2-canary1 for control1",
								},
							},
						},
						{
							Name:   "linux2",
							SHA256: IDs[1],
							Controls: []relaxting.ESInSpecReportControl{
								{
									ID:    "a2-dev2",
									Title: "a2-dev2 for control2",
								},
								{
									ID:    "a2-canary2",
									Title: "a2-canary2 for control2",
								},
							},
						},
					},
					EndTime: endTime,
				},
			},
			query: reporting.Query{
				Filters: []*reporting.ListFilter{
					{
						Type:   "end_time",
						Values: []string{time.Now().Format("2006-01-02T15:04:05Z")},
					},
					{
						Type:   "from",
						Values: []string{"0"},
					},
					{
						Type:   "size",
						Values: []string{"5"},
					},
					{
						Type:   "profile_id",
						Values: []string{IDs[3]},
					},
					{
						Type:   "control",
						Values: []string{"a2-dev4", "a2-canary3"},
					},
				},
			},
			expectedTitles:      []string{""},
			expectedResultCount: 1,
		},
		{
			description: "ListControlInfo: error when filtered with non existing control id",
			reports: []*relaxting.ESInSpecReport{
				{
					NodeID: "1",
					Profiles: []relaxting.ESInSpecReportProfile{
						{
							Name:   "linux1",
							SHA256: IDs[0],
							Controls: []relaxting.ESInSpecReportControl{
								{
									ID:    "a2-dev1",
									Title: "a2-dev1 for control1",
								},
								{
									ID:    "a2-canary1",
									Title: "a2-canary1 for control1",
								},
							},
						},
						{
							Name:   "linux2",
							SHA256: IDs[1],
							Controls: []relaxting.ESInSpecReportControl{
								{
									ID:    "a2-dev2",
									Title: "a2-dev2 for control2",
								},
								{
									ID:    "a2-canary2",
									Title: "a2-canary2 for control2",
								},
							},
						},
					},
					EndTime: endTime,
				},
			},
			query: reporting.Query{
				Filters: []*reporting.ListFilter{
					{
						Type:   "end_time",
						Values: []string{time.Now().Format("2006-01-02T15:04:05Z")},
					},
					{
						Type:   "from",
						Values: []string{"0"},
					},
					{
						Type:   "size",
						Values: []string{"5"},
					},
					{
						Type:   "profile_id",
						Values: []string{IDs[1]},
					},
					{
						Type:   "control",
						Values: []string{"a2-canary3"},
					},
				},
			},
			expectedTitles:      []string{""},
			expectedResultCount: 1,
		},
	}
	for _, test := range cases {
		t.Run(test.description, func(t *testing.T) {
			defer suite.DeleteAllDocuments()
			rIDs, err := suite.InsertInspecReports(test.reports)
			assert.NoError(t, err)
			require.NotNil(t, rIDs)
			test.query.Id = rIDs[0]
			_, err = server.ListControlInfo(ctx, &test.query)
			assert.Error(t, err)
		})
	}
}
