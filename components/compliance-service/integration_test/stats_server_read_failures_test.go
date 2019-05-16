package integration_test

import (
	"testing"

	iamV2 "github.com/chef/automate/api/interservice/authz/v2"
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

func TestReadFailures(t *testing.T) {
	reportFileName := "../ingest/examples/compliance-failure-big-report.json"
	everythingCtx := contextWithProjects([]string{authzConstants.AllProjectsExternalID})

	statsSvr := statsServer.New(&relaxting.ES2Backend{ESUrl: elasticsearchUrl})
	reportingSvr := reportingServer.New(&relaxting.ES2Backend{ESUrl: elasticsearchUrl})

	n := 5

	reportIds := make([]string, n)

	for i := 0; i < n; i++ {
		err := suite.ingestReport(reportFileName, func(r *compliance.Report) {
			id := newUUID()

			r.Environment = id
			r.NodeName = id
			r.NodeUuid = id
			r.Platform.Name = id
			r.Profiles[0].Controls = r.Profiles[0].Controls[2:3]
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
		response, _ := reportingSvr.ListReports(everythingCtx, &apiReporting.Query{})

		return response != nil && len(response.Reports) == n
	})

	reportsProjects := map[string][]string{
		"project1": reportIds[1:3],
		"project2": reportIds[2:5],
		"project3": reportIds[3:],
	}

	projectRules := map[string]*iamV2.ProjectRules{}
	for k, v := range reportsProjects {
		projectRules[k] = &iamV2.ProjectRules{
			Rules: []*iamV2.ProjectRule{
				{
					Conditions: []*iamV2.Condition{
						{
							Type:   iamV2.ProjectRuleConditionTypes_ROLES,
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

	aprilThirdQuery := stats.Query{
		Filters: []*stats.ListFilter{
			{Type: "start_time", Values: []string{"2017-04-02T23:59:59Z"}},
			{Type: "end_time", Values: []string{"2017-04-03T23:59:59Z"}},
		},
	}

	aprilThirdQueryWithAllFailureTypesQuery := aprilThirdQuery
	aprilThirdQueryWithAllFailureTypesQuery.Filters = append(aprilThirdQueryWithAllFailureTypesQuery.Filters,
		&stats.ListFilter{
			Type:   "types",
			Values: []string{"profile", "control", "environment", "platform"},
		})

	successCases := []struct {
		description       string
		allowedProjects   []string
		expectedFailedCnt int
	}{
		{
			description:     "Projects: user has access to all projects",
			allowedProjects: []string{authzConstants.AllProjectsExternalID},

			expectedFailedCnt: 5,
		},
		{
			description:     "Projects: user has access to one project with reports",
			allowedProjects: []string{"project1"},

			expectedFailedCnt: 2,
		},
		{
			description:     "Projects: user has access to some projects with reports",
			allowedProjects: []string{"project1", "project2"},

			expectedFailedCnt: 4,
		},
		{
			description:     "Projects: user has access to projects without reports",
			allowedProjects: []string{"project4", "project5"},

			expectedFailedCnt: 0,
		},
		{
			description:     "Projects: user has access to one project with reports and unassigned reports",
			allowedProjects: []string{"project1", authzConstants.UnassignedProjectID},

			expectedFailedCnt: 3,
		},
		{
			description:     "Projects: user has access to some projects with reports and unassigned reports",
			allowedProjects: []string{"project1", "project2", authzConstants.UnassignedProjectID},

			expectedFailedCnt: 5,
		},
		{
			description:     "Projects: user has access to projects without reports and unassigned reports",
			allowedProjects: []string{"project4", "project5", authzConstants.UnassignedProjectID},

			expectedFailedCnt: 1,
		},
		{
			description:     "Projects: user has access to unassigned reports",
			allowedProjects: []string{authzConstants.UnassignedProjectID},

			expectedFailedCnt: 1,
		},
	}

	for _, test := range successCases {
		t.Run(test.description, func(t *testing.T) {
			ctx := contextWithProjects(test.allowedProjects)

			response, err := statsSvr.ReadFailures(ctx, &aprilThirdQueryWithAllFailureTypesQuery)

			assert.NoError(t, err)
			require.NotNil(t, response)

			assert.Equal(t, test.expectedFailedCnt, len(response.Profiles), "Profiles Failures count")
			assert.Equal(t, test.expectedFailedCnt, len(response.Controls), "Controls Failures count")
			assert.Equal(t, test.expectedFailedCnt, len(response.Environments), "Environment Failures count")
			assert.Equal(t, test.expectedFailedCnt, len(response.Platforms), "Platforms Failures count")
		})
	}
}
