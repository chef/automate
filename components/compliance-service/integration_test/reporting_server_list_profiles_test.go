package integration_test

import (
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
	"testing"

	"github.com/chef/automate/api/interservice/authz"
	"github.com/chef/automate/api/interservice/compliance/ingest/events/compliance"
	"github.com/chef/automate/api/interservice/compliance/reporting"
	authzConstants "github.com/chef/automate/components/authz-service/constants"
	reportingServer "github.com/chef/automate/components/compliance-service/api/reporting/server"
	"github.com/chef/automate/components/compliance-service/inspec"
	"github.com/chef/automate/components/compliance-service/reporting/relaxting"
)

func TestListProfiles(t *testing.T) {
	suite.DeleteAllDocuments()

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
		response, _ := server.ListReports(everythingCtx, &reporting.Query{
			Filters: []*reporting.ListFilter{
				{Type: "end_time", Values: []string{"2018-10-25T18:18:41Z"}},
			},
		})

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

	projectRules := map[string]*authz.ProjectRules{}
	for k, v := range reportsProjects {
		projectRules[k] = &authz.ProjectRules{
			Rules: []*authz.ProjectRule{
				{
					Conditions: []*authz.Condition{
						{
							Attribute: authz.ProjectRuleConditionAttributes_CHEF_ROLE,
							Values:    v,
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

	suite.RefreshComplianceSummaryIndex()
	suite.RefreshComplianceReportIndex()
	suite.RefreshComplianceProfilesIndex()

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
			description:     "reporting_server_list_profiles_test.go => Projects: user has access to all projects",
			allowedProjects: []string{authzConstants.AllProjectsExternalID},
			expectedIds:     profileIds,
		},
		{
			description:     "reporting_server_list_profiles_test.go => Projects: user has access to one project with reports",
			allowedProjects: []string{"project1"},
			expectedIds:     profileIds[1:3],
		},
		{
			description:     "reporting_server_list_profiles_test.go => Projects: user has access to some projects with reports",
			allowedProjects: []string{"project1", "project2"},
			expectedIds:     profileIds[1:5],
		},
		{
			description:     "reporting_server_list_profiles_test.go => Projects: user has access to projects without reports",
			allowedProjects: []string{"project4", "project5"},
			expectedIds:     []string{},
		},
		{
			description:     "reporting_server_list_profiles_test.go => Projects: user has access to one project with reports and unassigned reports",
			allowedProjects: []string{"project1", authzConstants.UnassignedProjectID},
			expectedIds:     profileIds[:3],
		},
		{
			description:     "reporting_server_list_profiles_test.go => Projects: user has access to some projects with reports and unassigned reports",
			allowedProjects: []string{"project1", "project2", authzConstants.UnassignedProjectID},
			expectedIds:     profileIds[:5],
		},
		{
			description:     "reporting_server_list_profiles_test.go => Projects: user has access to projects without reports and unassigned reports",
			allowedProjects: []string{"project4", "project5", authzConstants.UnassignedProjectID},
			expectedIds:     profileIds[:1],
		},
		{
			description:     "reporting_server_list_profiles_test.go => Projects: user has access to unassigned reports",
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
