package integration_test

import (
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	authzConstants "github.com/chef/automate/components/authz-service/constants/v2"
	"github.com/chef/automate/components/compliance-service/api/reporting"
	reportingServer "github.com/chef/automate/components/compliance-service/api/reporting/server"
	"github.com/chef/automate/components/compliance-service/reporting/relaxting"
)

func TestListReports(t *testing.T) {
	server := reportingServer.New(&relaxting.ES2Backend{ESUrl: elasticsearchUrl})
	reports := []*relaxting.ESInSpecReport{
		{
			Projects: []string{},
		},
		{
			Projects: []string{"project1"},
		},
		{
			Projects: []string{"project1", "project2"},
		},
		{
			Projects: []string{"project2"},
		},
		{
			Projects: []string{"project2", "project3"},
		},
		{
			Projects: []string{"project3"},
		},
	}

	reportIds, err := suite.InsertInspecReports(reports)
	require.NoError(t, err)

	defer suite.DeleteAllDocuments()

	require.Len(t, reportIds, len(reports))

	successCases := []struct {
		description     string
		allowedProjects []string
		expectedIds     []string
	}{
		{
			description:     "Projects: user has access to all projects",
			allowedProjects: []string{authzConstants.AllProjectsExternalID},
			expectedIds:     reportIds,
		},
		{
			description:     "Projects: user has access to one project with reports",
			allowedProjects: []string{"project1"},
			expectedIds:     reportIds[1:3],
		},
		{
			description:     "Projects: user has access to some projects with reports",
			allowedProjects: []string{"project1", "project2"},
			expectedIds:     reportIds[1:5],
		},
		{
			description:     "Projects: user has access to projects without reports",
			allowedProjects: []string{"project4", "project5"},
			expectedIds:     []string{},
		},
		{
			description:     "Projects: user has access to one project with reports and unassigned reports",
			allowedProjects: []string{"project1", authzConstants.UnassignedProjectID},
			expectedIds:     reportIds[:3],
		},
		{
			description:     "Projects: user has access to some projects with reports and unassigned reports",
			allowedProjects: []string{"project1", "project2", authzConstants.UnassignedProjectID},
			expectedIds:     reportIds[:5],
		},
		{
			description:     "Projects: user has access to projects without reports and unassigned reports",
			allowedProjects: []string{"project4", "project5", authzConstants.UnassignedProjectID},
			expectedIds:     reportIds[:1],
		},
		{
			description:     "Projects: user has access to unassigned reports",
			allowedProjects: []string{authzConstants.UnassignedProjectID},
			expectedIds:     reportIds[:1],
		},
	}

	for _, test := range successCases {
		t.Run(test.description, func(t *testing.T) {
			ctx := contextWithProjects(test.allowedProjects)

			response, err := server.ListReports(ctx, &reporting.Query{})

			assert.NoError(t, err)
			require.NotNil(t, response)

			actualIds := make([]string, len(response.Reports))
			for i, report := range response.Reports {
				actualIds[i] = report.Id
			}

			assert.ElementsMatch(t, test.expectedIds, actualIds)
		})
	}
}
