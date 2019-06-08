package integration_test

import (
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	iam_v2 "github.com/chef/automate/api/interservice/authz/v2"
	authzConstants "github.com/chef/automate/components/authz-service/constants/v2"
	"github.com/chef/automate/components/compliance-service/api/reporting"
	reportingServer "github.com/chef/automate/components/compliance-service/api/reporting/server"
	"github.com/chef/automate/components/compliance-service/ingest/events/compliance"
	"github.com/chef/automate/components/compliance-service/inspec"
	"github.com/chef/automate/components/compliance-service/reporting/relaxting"
)

func TestListProfiles(t *testing.T) {
	es2Backend := relaxting.ES2Backend{ESUrl: elasticsearchUrl}
	server := reportingServer.New(&es2Backend)

	reportFileName := "../ingest/examples/compliance-success-tiny-report.json"
	everythingCtx := contextWithProjects([]string{authzConstants.AllProjectsExternalID})

	n := 5

	reportIds := make([]string, n)

	for i := 0; i < n; i++ {
		err := suite.ingestReport(reportFileName, func(r *compliance.Report) {
			id := newUUID()

			r.Profiles[0].Controls = r.Profiles[0].Controls[:1]
			r.Profiles[0].Controls[0].Id = id
			r.Profiles = r.Profiles[:1]
			r.Profiles[0].Sha256 = id
			r.ReportUuid = id
			r.Roles = []string{id}

			reportIds[i] = id
		})

		require.NoError(t, err)
	}

	defer suite.DeleteAllDocuments()

	waitFor(func() bool {
		response, _ := server.ListReports(everythingCtx, &reporting.Query{})

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
	require.NoError(t, err)

	suite.WaitForESJobToComplete(esJobID)

	suite.RefreshComplianceReportIndex()

	profileIds := make([]string, len(reportIds))
	for i := range reportIds {
		profileId := reportIds[i]
		profileIds[i] = profileId

		profile := inspec.Profile{
			Name:    "a",
			Title:   "b",
			Version: "1",
			Sha256:  profileId,
		}

		err := es2Backend.StoreProfile(profile)
		require.NoError(t, err)
	}

	successCases := []struct {
		description     string
		allowedProjects []string
		expectedIds     []string
	}{
		{
			description:     "Projects: user has access to all projects",
			allowedProjects: []string{authzConstants.AllProjectsExternalID},
			expectedIds:     profileIds,
		},
		{
			description:     "Projects: user has access to one project with reports",
			allowedProjects: []string{"project1"},
			expectedIds:     profileIds[1:3],
		},
		{
			description:     "Projects: user has access to some projects with reports",
			allowedProjects: []string{"project1", "project2"},
			expectedIds:     profileIds[1:5],
		},
		{
			description:     "Projects: user has access to projects without reports",
			allowedProjects: []string{"project4", "project5"},
			expectedIds:     []string{},
		},
		{
			description:     "Projects: user has access to one project with reports and unassigned reports",
			allowedProjects: []string{"project1", authzConstants.UnassignedProjectID},
			expectedIds:     profileIds[:3],
		},
		{
			description:     "Projects: user has access to some projects with reports and unassigned reports",
			allowedProjects: []string{"project1", "project2", authzConstants.UnassignedProjectID},
			expectedIds:     profileIds[:5],
		},
		{
			description:     "Projects: user has access to projects without reports and unassigned reports",
			allowedProjects: []string{"project4", "project5", authzConstants.UnassignedProjectID},
			expectedIds:     profileIds[:1],
		},
		{
			description:     "Projects: user has access to unassigned reports",
			allowedProjects: []string{authzConstants.UnassignedProjectID},
			expectedIds:     profileIds[:1],
		},
	}

	for _, test := range successCases {
		t.Run(test.description, func(t *testing.T) {
			ctx := contextWithProjects(test.allowedProjects)

			query := reporting.Query{Filters: []*reporting.ListFilter{{Type: "end_time", Values: []string{"2018-10-25T23:59:59Z"}}}}

			response, err := server.ListProfiles(ctx, &query)

			assert.NoError(t, err)
			require.NotNil(t, response)

			assert.Equal(t, len(test.expectedIds), int(response.Counts.Total))

			actualIds := make([]string, len(response.Profiles))
			for i, profile := range response.Profiles {
				actualIds[i] = profile.Id
			}

			assert.ElementsMatch(t, test.expectedIds, actualIds)
		})
	}
}
