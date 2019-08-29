package integration_test

import (
	"testing"

	iam_v2 "github.com/chef/automate/api/interservice/authz/v2"
	"github.com/chef/automate/components/compliance-service/ingest/events/compliance"

	apiReporting "github.com/chef/automate/components/compliance-service/api/reporting"
	reportingServer "github.com/chef/automate/components/compliance-service/api/reporting/server"
	"github.com/chef/automate/components/compliance-service/api/stats"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	authzConstants "github.com/chef/automate/components/authz-service/constants/v2"
	statsServer "github.com/chef/automate/components/compliance-service/api/stats/server"
	"github.com/chef/automate/components/compliance-service/reporting/relaxting"
)

var octoberTwentyFifthQuery = stats.Query{
	Filters: []*stats.ListFilter{
		{Type: "start_time", Values: []string{"2018-10-24T23:59:59Z"}},
		{Type: "end_time", Values: []string{"2018-10-25T23:59:59Z"}},
	},
}

func TestReadProfilesList(t *testing.T) {
	statsServer := setupReadProfiles(t)
	defer suite.DeleteAllDocuments()

	successCases := []struct {
		description         string
		allowedProjects     []string
		expectedProfileList []*stats.ProfileList
	}{
		{
			description:     "Projects: user has access to all projects",
			allowedProjects: []string{authzConstants.AllProjectsExternalID},

			expectedProfileList: []*stats.ProfileList{{Name: "mylinux-success",
				Id: "1de944869a847da87d3774feaacb41829935a2f46b558f7fc34b4da21586ae27", Passed: 5}},
		},
		{
			description:     "Projects: user has access to one project with reports",
			allowedProjects: []string{"project1"},

			expectedProfileList: []*stats.ProfileList{{Name: "mylinux-success",
				Id: "1de944869a847da87d3774feaacb41829935a2f46b558f7fc34b4da21586ae27", Passed: 2}},
		},
		{
			description:     "Projects: user has access to some projects with reports",
			allowedProjects: []string{"project1", "project2"},

			expectedProfileList: []*stats.ProfileList{{Name: "mylinux-success",
				Id: "1de944869a847da87d3774feaacb41829935a2f46b558f7fc34b4da21586ae27", Passed: 4}},
		},
		{
			description:     "Projects: user has access to projects without reports",
			allowedProjects: []string{"project4", "project5"},

			expectedProfileList: []*stats.ProfileList{},
		},
		{
			description:     "Projects: user has access to one project with reports and unassigned reports",
			allowedProjects: []string{"project1", authzConstants.UnassignedProjectID},

			expectedProfileList: []*stats.ProfileList{{Name: "mylinux-success",
				Id: "1de944869a847da87d3774feaacb41829935a2f46b558f7fc34b4da21586ae27", Passed: 3}},
		},
		{
			description:     "Projects: user has access to some projects with reports and unassigned reports",
			allowedProjects: []string{"project1", "project2", authzConstants.UnassignedProjectID},

			expectedProfileList: []*stats.ProfileList{{Name: "mylinux-success",
				Id: "1de944869a847da87d3774feaacb41829935a2f46b558f7fc34b4da21586ae27", Passed: 5}},
		},
		{
			description:     "Projects: user has access to projects without reports and unassigned reports",
			allowedProjects: []string{"project4", "project5", authzConstants.UnassignedProjectID},

			expectedProfileList: []*stats.ProfileList{{Name: "mylinux-success",
				Id: "1de944869a847da87d3774feaacb41829935a2f46b558f7fc34b4da21586ae27", Passed: 1}},
		},
		{
			description:     "Projects: user has access to unassigned reports",
			allowedProjects: []string{authzConstants.UnassignedProjectID},

			expectedProfileList: []*stats.ProfileList{{Name: "mylinux-success",
				Id: "1de944869a847da87d3774feaacb41829935a2f46b558f7fc34b4da21586ae27", Passed: 1}},
		},
	}

	for _, test := range successCases {
		t.Run(test.description, func(t *testing.T) {
			ctx := contextWithProjects(test.allowedProjects)

			query := octoberTwentyFifthQuery

			response, err := statsServer.ReadProfiles(ctx, &query)

			assert.NoError(t, err)
			require.NotNil(t, response)

			assert.Equal(t, test.expectedProfileList, response.ProfileList,
				"No ID passed in Profiles List")
		})
	}
}

func TestReadProfileSummary(t *testing.T) {
	statsServer := setupReadProfiles(t)
	defer suite.DeleteAllDocuments()

	successCases := []struct {
		description            string
		allowedProjects        []string
		expectedProfileSummary *stats.ProfileSummary
	}{
		{
			description:     "Projects: user has access to all projects",
			allowedProjects: []string{authzConstants.AllProjectsExternalID},

			expectedProfileSummary: &stats.ProfileSummary{
				Name:    "mylinux-success",
				Version: "1.8.9",
				Stats: &stats.ProfileSummaryStats{
					Passed:      5,
					Failed:      0,
					Skipped:     0,
					FailedNodes: 0,
					TotalNodes:  5,
				},
			},
		},
		{
			description:     "Projects: user has access to one project with reports",
			allowedProjects: []string{"project1"},

			expectedProfileSummary: &stats.ProfileSummary{
				Name:    "mylinux-success",
				Version: "1.8.9",
				Stats: &stats.ProfileSummaryStats{
					Passed:      2,
					Failed:      0,
					Skipped:     0,
					FailedNodes: 0,
					TotalNodes:  2,
				},
			},
		},
		{
			description:     "Projects: user has access to some projects with reports",
			allowedProjects: []string{"project1", "project2"},

			expectedProfileSummary: &stats.ProfileSummary{
				Name:    "mylinux-success",
				Version: "1.8.9",
				Stats: &stats.ProfileSummaryStats{
					Passed:      4,
					Failed:      0,
					Skipped:     0,
					FailedNodes: 0,
					TotalNodes:  4,
				},
			},
		},
		{
			description:     "Projects: user has access to projects without reports",
			allowedProjects: []string{"project4", "project5"},

			expectedProfileSummary: &stats.ProfileSummary{
				Name:    "mylinux-success",
				Version: "1.8.9",
				Stats: &stats.ProfileSummaryStats{
					Passed:      0,
					Failed:      0,
					Skipped:     0,
					FailedNodes: 0,
					TotalNodes:  0,
				},
			},
		},
		{
			description:     "Projects: user has access to one project with reports and unassigned reports",
			allowedProjects: []string{"project1", authzConstants.UnassignedProjectID},

			expectedProfileSummary: &stats.ProfileSummary{
				Name:    "mylinux-success",
				Version: "1.8.9",
				Stats: &stats.ProfileSummaryStats{
					Passed:      3,
					Failed:      0,
					Skipped:     0,
					FailedNodes: 0,
					TotalNodes:  3,
				},
			},
		},
		{
			description:     "Projects: user has access to some projects with reports and unassigned reports",
			allowedProjects: []string{"project1", "project2", authzConstants.UnassignedProjectID},

			expectedProfileSummary: &stats.ProfileSummary{
				Name:    "mylinux-success",
				Version: "1.8.9",
				Stats: &stats.ProfileSummaryStats{
					Passed:      5,
					Failed:      0,
					Skipped:     0,
					FailedNodes: 0,
					TotalNodes:  5,
				},
			},
		},
		{
			description:     "Projects: user has access to projects without reports and unassigned reports",
			allowedProjects: []string{"project4", "project5", authzConstants.UnassignedProjectID},

			expectedProfileSummary: &stats.ProfileSummary{
				Name:    "mylinux-success",
				Version: "1.8.9",
				Stats: &stats.ProfileSummaryStats{
					Passed:      1,
					Failed:      0,
					Skipped:     0,
					FailedNodes: 0,
					TotalNodes:  1,
				},
			},
		},
		{
			description:     "Projects: user has access to unassigned reports",
			allowedProjects: []string{authzConstants.UnassignedProjectID},

			expectedProfileSummary: &stats.ProfileSummary{
				Name:    "mylinux-success",
				Version: "1.8.9",
				Stats: &stats.ProfileSummaryStats{
					Passed:      1,
					Failed:      0,
					Skipped:     0,
					FailedNodes: 0,
					TotalNodes:  1,
				},
			},
		},
	}

	for _, test := range successCases {
		t.Run(test.description, func(t *testing.T) {
			ctx := contextWithProjects(test.allowedProjects)

			query := octoberTwentyFifthQuery

			query.Type = "summary"
			query.Id = "1de944869a847da87d3774feaacb41829935a2f46b558f7fc34b4da21586ae27"

			response, err := statsServer.ReadProfiles(ctx, &query)

			assert.NoError(t, err)
			require.NotNil(t, response)

			assert.Equal(t, test.expectedProfileSummary.Stats.Passed, response.ProfileSummary.Stats.Passed,
				"No ID passed in Profiles List")

			assert.Equal(t, test.expectedProfileSummary.Stats.Failed, response.ProfileSummary.Stats.Failed,
				"No ID Failed in Profiles List")
		})
	}
}

func TestReadProfilesControlStats(t *testing.T) {
	statsServer := setupReadProfiles(t)
	defer suite.DeleteAllDocuments()

	successCases := []struct {
		description          string
		allowedProjects      []string
		expectedControlStats []*stats.ControlStats
	}{
		{
			description:     "Projects: user has access to all projects",
			allowedProjects: []string{authzConstants.AllProjectsExternalID},

			expectedControlStats: []*stats.ControlStats{{
				Control: "/etc/passwd must exist",
				Title:   "Checking for /etc/passwd",
				Passed:  5,
				Failed:  0,
				Skipped: 0,
				Impact:  0.6,
			}},
		},
		{
			description:     "Projects: user has access to one project with reports",
			allowedProjects: []string{"project1"},

			expectedControlStats: []*stats.ControlStats{{
				Control: "/etc/passwd must exist",
				Title:   "Checking for /etc/passwd",
				Passed:  2,
				Failed:  0,
				Skipped: 0,
				Impact:  0.6,
			}},
		},
		{
			description:     "Projects: user has access to some projects with reports",
			allowedProjects: []string{"project1", "project2"},

			expectedControlStats: []*stats.ControlStats{{
				Control: "/etc/passwd must exist",
				Title:   "Checking for /etc/passwd",
				Passed:  4,
				Failed:  0,
				Skipped: 0,
				Impact:  0.6,
			}},
		},
		{
			description:          "Projects: user has access to projects without reports",
			allowedProjects:      []string{"project4", "project5"},
			expectedControlStats: []*stats.ControlStats{},
		},
		{
			description:     "Projects: user has access to one project with reports and unassigned reports",
			allowedProjects: []string{"project1", authzConstants.UnassignedProjectID},

			expectedControlStats: []*stats.ControlStats{{
				Control: "/etc/passwd must exist",
				Title:   "Checking for /etc/passwd",
				Passed:  3,
				Failed:  0,
				Skipped: 0,
				Impact:  0.6,
			}},
		},
		{
			description:     "Projects: user has access to some projects with reports and unassigned reports",
			allowedProjects: []string{"project1", "project2", authzConstants.UnassignedProjectID},

			expectedControlStats: []*stats.ControlStats{{
				Control: "/etc/passwd must exist",
				Title:   "Checking for /etc/passwd",
				Passed:  5,
				Failed:  0,
				Skipped: 0,
				Impact:  0.6,
			}},
		},
		{
			description:     "Projects: user has access to projects without reports and unassigned reports",
			allowedProjects: []string{"project4", "project5", authzConstants.UnassignedProjectID},

			expectedControlStats: []*stats.ControlStats{{
				Control: "/etc/passwd must exist",
				Title:   "Checking for /etc/passwd",
				Passed:  1,
				Failed:  0,
				Skipped: 0,
				Impact:  0.6,
			}},
		},
		{
			description:     "Projects: user has access to unassigned reports",
			allowedProjects: []string{authzConstants.UnassignedProjectID},

			expectedControlStats: []*stats.ControlStats{{
				Control: "/etc/passwd must exist",
				Title:   "Checking for /etc/passwd",
				Passed:  1,
				Failed:  0,

				Skipped: 0,
				Impact:  0.6,
			}},
		},
	}

	for _, test := range successCases {
		t.Run(test.description, func(t *testing.T) {
			ctx := contextWithProjects(test.allowedProjects)

			query := octoberTwentyFifthQuery

			query.Type = "controls"
			query.Id = "1de944869a847da87d3774feaacb41829935a2f46b558f7fc34b4da21586ae27"

			response, err := statsServer.ReadProfiles(ctx, &query)

			assert.NoError(t, err)
			require.NotNil(t, response)

			assert.Equal(t, test.expectedControlStats, response.ControlStats,
				"No ID passed in Profiles List")
		})
	}
}

func setupReadProfiles(t *testing.T) *statsServer.Server {
	reportFileName := "../ingest/examples/compliance-success-tiny-report.json"
	everythingCtx := contextWithProjects([]string{authzConstants.AllProjectsExternalID})
	statsServer := statsServer.New(&relaxting.ES2Backend{ESUrl: elasticsearchUrl})
	reportingServer := reportingServer.New(&relaxting.ES2Backend{ESUrl: elasticsearchUrl})
	n := 5
	reportIds := make([]string, n)
	for i := 0; i < n; i++ {
		err := suite.ingestReport(reportFileName, func(r *compliance.Report) {
			id := newUUID()

			r.Environment = id
			r.NodeName = id
			r.NodeUuid = id
			r.Platform.Name = id
			r.Profiles[0].Controls = r.Profiles[0].Controls[:1]
			r.Profiles = r.Profiles[:1]
			r.Profiles[0].Title = id
			r.Recipes = []string{id}
			r.ReportUuid = id
			r.Roles = []string{id}

			reportIds[i] = id
		})

		require.NoError(t, err)
	}
	waitFor(func() bool {
		response, _ := reportingServer.ListReports(everythingCtx, &apiReporting.Query{})

		return response != nil && len(response.Reports) == n
	})

	suite.RefreshComplianceSummaryIndex()
	suite.RefreshComplianceReportIndex()
	suite.RefreshComplianceProfilesIndex()

	reportsProjects := map[string][]string{
		"project1": reportIds[1:3],
		"project2": reportIds[2:5],
		"project3": reportIds[3:],
	}
	projectRules := map[string]*iam_v2.ProjectRules{}
	for k, v := range reportsProjects {
		projectRules[k] = &iam_v2.ProjectRules{
			Rules: []*iam_v2.ProjectRule{
				{
					Conditions: []*iam_v2.Condition{
						{
							Attribute: iam_v2.ProjectRuleConditionAttributes_CHEF_ROLE,
							Values:    v,
						},
					},
				},
			},
		}
	}
	// Send a project rules update event
	esJobID, err := suite.ingesticESClient.UpdateReportProjectsTags(everythingCtx, projectRules)
	assert.Nil(t, err)
	suite.WaitForESJobToComplete(esJobID)
	suite.RefreshComplianceSummaryIndex()
	suite.RefreshComplianceReportIndex()
	suite.RefreshComplianceProfilesIndex()
	return statsServer
}
