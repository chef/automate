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

func TestReadTrend(t *testing.T) {
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
			r.Profiles[0].Controls[0].Id = id
			r.Profiles[0].Controls[0].Title = id
			r.Profiles = r.Profiles[:1]
			r.Profiles[0].Sha256 = id
			r.Profiles[0].Title = id
			r.Recipes = []string{id}
			r.ReportUuid = id
			r.Roles = []string{id}

			reportIds[i] = id
		})

		require.NoError(t, err)
	}

	defer suite.DeleteAllDocuments()

	waitFor(func() bool {
		response, _ := reportingServer.ListReports(everythingCtx, &apiReporting.Query{})

		return response != nil && len(response.Reports) == n
	})

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
							Type:   iam_v2.ProjectRuleConditionTypes_ROLES,
							Values: v,
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

	suite.RefreshComplianceReportIndex()

	octoberTwentyFifthQuery := stats.Query{
		Filters: []*stats.ListFilter{
			{Type: "start_time", Values: []string{"2018-10-24T23:59:59Z"}},
			{Type: "end_time", Values: []string{"2018-10-25T23:59:59Z"}},
		},
	}
	successCases := []struct {
		description     string
		allowedProjects []string
		query           stats.Query

		expectedPassedCnt int32
	}{
		{
			description:     "Projects: user has access to all projects",
			allowedProjects: []string{authzConstants.AllProjectsExternalID},
			query:           octoberTwentyFifthQuery,

			expectedPassedCnt: 5,
		},
		{
			description:     "Projects: user has access to one project with reports",
			allowedProjects: []string{"project1"},
			query:           octoberTwentyFifthQuery,

			expectedPassedCnt: 2,
		},
		{
			description:     "Projects: user has access to some projects with reports",
			allowedProjects: []string{"project1", "project2"},
			query:           octoberTwentyFifthQuery,

			expectedPassedCnt: 4,
		},
		{
			description:     "Projects: user has access to projects without reports",
			allowedProjects: []string{"project4", "project5"},
			query:           octoberTwentyFifthQuery,

			expectedPassedCnt: 0,
		},
		{
			description:     "Projects: user has access to one project with reports and unassigned reports",
			allowedProjects: []string{"project1", authzConstants.UnassignedProjectID},
			query:           octoberTwentyFifthQuery,

			expectedPassedCnt: 3,
		},
		{
			description:     "Projects: user has access to some projects with reports and unassigned reports",
			allowedProjects: []string{"project1", "project2", authzConstants.UnassignedProjectID},
			query:           octoberTwentyFifthQuery,

			expectedPassedCnt: 5,
		},
		{
			description:     "Projects: user has access to projects without reports and unassigned reports",
			allowedProjects: []string{"project4", "project5", authzConstants.UnassignedProjectID},
			query:           octoberTwentyFifthQuery,

			expectedPassedCnt: 1,
		},
		{
			description:     "Projects: user has access to unassigned reports",
			allowedProjects: []string{authzConstants.UnassignedProjectID},
			query:           octoberTwentyFifthQuery,

			expectedPassedCnt: 1,
		},
	}

	for _, test := range successCases {
		t.Run(test.description, func(t *testing.T) {
			ctx := contextWithProjects(test.allowedProjects)

			test.query.Type = "nodes"
			response, err := statsServer.ReadTrend(ctx, &test.query)

			assert.NoError(t, err)
			require.NotNil(t, response)

			assert.Equal(t, test.expectedPassedCnt, response.Trends[0].Passed, "Nodes Passed count")

			test.query.Type = "controls"
			response, err = statsServer.ReadTrend(ctx, &test.query)

			assert.NoError(t, err)
			require.NotNil(t, response)

			//controls trends
			assert.Equal(t, test.expectedPassedCnt, response.Trends[0].Passed, "Controls Passed count")
		})
	}
}
