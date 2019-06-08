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

func TestReadReportSummary(t *testing.T) {
	statsServer := setupReadSummary(t)
	defer suite.DeleteAllDocuments()

	successCases := []struct {
		description     string
		allowedProjects []string

		//report summary
		expectedEnvironmentCnt int32
		expectedPlatformCnt    int32
		expectedProfileCnt     int32
		expectedNodeCnt        int64
		expectedStatus         string
	}{
		{
			description:     "Projects: user has access to all projects",
			allowedProjects: []string{authzConstants.AllProjectsExternalID},

			//report summary
			expectedEnvironmentCnt: 5,
			expectedPlatformCnt:    5,
			expectedProfileCnt:     5,
			expectedNodeCnt:        5,
			expectedStatus:         "passed",
		},
		{
			description:     "Projects: user has access to one project with reports",
			allowedProjects: []string{"project1"},

			//report summary
			expectedEnvironmentCnt: 2,
			expectedPlatformCnt:    2,
			expectedProfileCnt:     2,
			expectedNodeCnt:        2,
			expectedStatus:         "passed",
		},
		{
			description:     "Projects: user has access to some projects with reports",
			allowedProjects: []string{"project1", "project2"},

			//report summary
			expectedEnvironmentCnt: 4,
			expectedPlatformCnt:    4,
			expectedProfileCnt:     4,
			expectedNodeCnt:        4,
			expectedStatus:         "passed",
		},
		{
			description:     "Projects: user has access to projects without reports",
			allowedProjects: []string{"project4", "project5"},

			//report summary
			expectedEnvironmentCnt: 0,
			expectedPlatformCnt:    0,
			expectedProfileCnt:     0,
			expectedNodeCnt:        0,
			expectedStatus:         "unknown",
		},
		{
			description:     "Projects: user has access to one project with reports and unassigned reports",
			allowedProjects: []string{"project1", authzConstants.UnassignedProjectID},

			//report summary
			expectedEnvironmentCnt: 3,
			expectedPlatformCnt:    3,
			expectedProfileCnt:     3,
			expectedNodeCnt:        3,
			expectedStatus:         "passed",
		},
		{
			description:     "Projects: user has access to some projects with reports and unassigned reports",
			allowedProjects: []string{"project1", "project2", authzConstants.UnassignedProjectID},

			//report summary
			expectedEnvironmentCnt: 5,
			expectedPlatformCnt:    5,
			expectedProfileCnt:     5,
			expectedNodeCnt:        5,
			expectedStatus:         "passed",
		},
		{
			description:     "Projects: user has access to projects without reports and unassigned reports",
			allowedProjects: []string{"project4", "project5", authzConstants.UnassignedProjectID},

			//report summary
			expectedEnvironmentCnt: 1,
			expectedPlatformCnt:    1,
			expectedProfileCnt:     1,
			expectedNodeCnt:        1,
			expectedStatus:         "passed",
		},
		{
			description:     "Projects: user has access to unassigned reports",
			allowedProjects: []string{authzConstants.UnassignedProjectID},

			//report summary
			expectedEnvironmentCnt: 1,
			expectedPlatformCnt:    1,
			expectedProfileCnt:     1,
			expectedNodeCnt:        1,
			expectedStatus:         "passed",
		},
	}

	for _, test := range successCases {
		t.Run(test.description, func(t *testing.T) {
			ctx := contextWithProjects(test.allowedProjects)

			octoberTwentyFifthQuery := &stats.Query{
				Filters: []*stats.ListFilter{
					{Type: "end_time", Values: []string{"2018-10-25T23:59:59Z"}},
				},
			}
			//passing in no type gets us a Summary type that contains a hydrated ReportSummary
			response, err := statsServer.ReadSummary(ctx, octoberTwentyFifthQuery)

			assert.NoError(t, err)
			require.NotNil(t, response)

			reportSummary := response.ReportSummary

			//report summary
			assert.Equal(t, test.expectedEnvironmentCnt, reportSummary.Stats.Environments, "Env count")
			assert.Equal(t, test.expectedPlatformCnt, reportSummary.Stats.Platforms, "Platforms count")
			assert.Equal(t, test.expectedProfileCnt, reportSummary.Stats.Profiles, "Profiles count")
			assert.Equal(t, test.expectedNodeCnt, reportSummary.Stats.Nodes, "Nodes count")
			assert.Equal(t, test.expectedStatus, reportSummary.Status, "status")
		})
	}
}

func TestReadNodeSummary(t *testing.T) {
	statsServer := setupReadSummary(t)
	defer suite.DeleteAllDocuments()

	successCases := []struct {
		description     string
		allowedProjects []string

		//node summary
		expectedCompliantCnt    int32
		expectedNonCompliantCnt int32
		expectedHighRiskCnt     int32
		expectedLowRiskCnt      int32
		expectedMediumRiskCnt   int32
		expectedSkippedCnt      int32
	}{
		{
			description:     "Projects: user has access to all projects",
			allowedProjects: []string{authzConstants.AllProjectsExternalID},

			//nodes summary
			expectedCompliantCnt: 5,
		},
		{
			description:     "Projects: user has access to one project with reports",
			allowedProjects: []string{"project1"},

			//nodes summary
			expectedCompliantCnt: 2,
		},
		{
			description:     "Projects: user has access to some projects with reports",
			allowedProjects: []string{"project1", "project2"},

			//nodes summary
			expectedCompliantCnt:    4,
			expectedNonCompliantCnt: 0,
		},
		{
			description:     "Projects: user has access to projects without reports",
			allowedProjects: []string{"project4", "project5"},

			//nodes summary
			expectedCompliantCnt:    0,
			expectedNonCompliantCnt: 0,
		},
		{
			description:     "Projects: user has access to one project with reports and unassigned reports",
			allowedProjects: []string{"project1", authzConstants.UnassignedProjectID},

			//nodes summary
			expectedCompliantCnt:    3,
			expectedNonCompliantCnt: 0,
		},
		{
			description:     "Projects: user has access to some projects with reports and unassigned reports",
			allowedProjects: []string{"project1", "project2", authzConstants.UnassignedProjectID},

			//nodes summary
			expectedCompliantCnt:    5,
			expectedNonCompliantCnt: 0,
		},
		{
			description:     "Projects: user has access to projects without reports and unassigned reports",
			allowedProjects: []string{"project4", "project5", authzConstants.UnassignedProjectID},

			//nodes summary
			expectedCompliantCnt:    1,
			expectedNonCompliantCnt: 0,
		},
		{
			description:     "Projects: user has access to unassigned reports",
			allowedProjects: []string{authzConstants.UnassignedProjectID},

			//nodes summary
			expectedCompliantCnt:    1,
			expectedNonCompliantCnt: 0,
		},
	}

	for _, test := range successCases {
		t.Run(test.description, func(t *testing.T) {
			ctx := contextWithProjects(test.allowedProjects)

			octoberTwentyFifthQuery := &stats.Query{
				Filters: []*stats.ListFilter{
					{Type: "end_time", Values: []string{"2018-10-25T23:59:59Z"}},
				},
			}

			//passing in "nodes" type gets us a Summary type that contains a hydrated NodeSummary
			octoberTwentyFifthQuery.Type = "nodes"
			response, err := statsServer.ReadSummary(ctx, octoberTwentyFifthQuery)

			assert.NoError(t, err)
			require.NotNil(t, response)

			nodeSummary := response.NodeSummary

			//node summary
			assert.Equal(t, test.expectedCompliantCnt, nodeSummary.Compliant, "Compliant count")
			assert.Equal(t, test.expectedNonCompliantCnt, nodeSummary.Noncompliant, "NonCompliant count")
			assert.Equal(t, test.expectedHighRiskCnt, nodeSummary.HighRisk, "HighRisk count")
			assert.Equal(t, test.expectedLowRiskCnt, nodeSummary.LowRisk, "LowRisk count")
			assert.Equal(t, test.expectedMediumRiskCnt, nodeSummary.MediumRisk, "MediumRisk count")
			assert.Equal(t, test.expectedSkippedCnt, nodeSummary.Skipped, "Skipped count")
		})
	}
}

func TestReadControlSummary(t *testing.T) {
	statsServer := setupReadSummary(t)
	defer suite.DeleteAllDocuments()

	successCases := []struct {
		description     string
		allowedProjects []string

		//control summary
		expectedPassedCnt int32
	}{
		{
			description:     "Projects: user has access to all projects",
			allowedProjects: []string{authzConstants.AllProjectsExternalID},

			//controls summary
			expectedPassedCnt: 5,
		},
		{
			description:     "Projects: user has access to one project with reports",
			allowedProjects: []string{"project1"},

			//controls summary
			expectedPassedCnt: 2,
		},
		{
			description:     "Projects: user has access to some projects with reports",
			allowedProjects: []string{"project1", "project2"},

			//controls summary
			expectedPassedCnt: 4,
		},
		{
			description:     "Projects: user has access to projects without reports",
			allowedProjects: []string{"project4", "project5"},

			//controls summary
			expectedPassedCnt: 0,
		},
		{
			description:     "Projects: user has access to one project with reports and unassigned reports",
			allowedProjects: []string{"project1", authzConstants.UnassignedProjectID},

			//controls summary
			expectedPassedCnt: 3,
		},
		{
			description:     "Projects: user has access to some projects with reports and unassigned reports",
			allowedProjects: []string{"project1", "project2", authzConstants.UnassignedProjectID},

			//controls summary
			expectedPassedCnt: 5,
		},
		{
			description:     "Projects: user has access to projects without reports and unassigned reports",
			allowedProjects: []string{"project4", "project5", authzConstants.UnassignedProjectID},

			//controls summary
			expectedPassedCnt: 1,
		},
		{
			description:     "Projects: user has access to unassigned reports",
			allowedProjects: []string{authzConstants.UnassignedProjectID},

			//controls summary
			expectedPassedCnt: 1,
		},
	}

	for _, test := range successCases {
		t.Run(test.description, func(t *testing.T) {
			ctx := contextWithProjects(test.allowedProjects)

			octoberTwentyFifthQuery := &stats.Query{
				Filters: []*stats.ListFilter{
					{Type: "end_time", Values: []string{"2018-10-25T23:59:59Z"}},
				},
			}
			//passing in "controls" type gets us a Summary type that contains a hydrated ControlSummary
			octoberTwentyFifthQuery.Type = "controls"
			response, err := statsServer.ReadSummary(ctx, octoberTwentyFifthQuery)

			assert.NoError(t, err)
			require.NotNil(t, response)

			controlSummary := response.ControlsSummary

			//control summary
			assert.Equal(t, test.expectedPassedCnt, controlSummary.Passed, "Passed count")
		})
	}
}

func setupReadSummary(t *testing.T) *statsServer.Server {
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
							Type:   iam_v2.ProjectRuleConditionAttributes_ROLES,
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
	return statsServer
}
