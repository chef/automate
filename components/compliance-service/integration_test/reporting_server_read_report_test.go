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

func TestReadReport(t *testing.T) {
	server := reportingServer.New(&relaxting.ES2Backend{ESUrl: elasticsearchUrl})
	reports := []*relaxting.ESInSpecReport{
		{
			Projects: []string{},
		},
		{
			Projects: []string{"project1", "project2"},
		},
	}
	reportIds, err := suite.InsertInspecReports(reports)
	require.NoError(t, err)

	defer suite.DeleteAllDocuments()

	require.Len(t, reportIds, 2)

	unassignedReportId := reportIds[0]
	assignedReportId := reportIds[1]

	successCases := []struct {
		description     string
		allowedProjects []string
		reportId        string
	}{
		{
			description:     "Projects: user has access to all projects accessing an assigned report",
			allowedProjects: []string{authzConstants.AllProjectsExternalID},
			reportId:        assignedReportId,
		},
		{
			description:     "Projects: user has access to all projects accessing an unassigned report",
			allowedProjects: []string{authzConstants.AllProjectsExternalID},
			reportId:        unassignedReportId,
		},
		{
			description:     "Projects: user has access to all projects a report belongs to accessing an assigned report",
			allowedProjects: []string{"project1", "project2"},
			reportId:        assignedReportId,
		},
		{
			description:     "Projects: user has access to some projects a report belongs to accessing an assigned report",
			allowedProjects: []string{"project1", "project3"},
			reportId:        assignedReportId,
		},
		{
			description:     "Projects: user has access to unassigned reports accessing an unassigned report",
			allowedProjects: []string{"project1", authzConstants.UnassignedProjectID},
			reportId:        unassignedReportId,
		},
	}

	for _, test := range successCases {
		t.Run(test.description, func(t *testing.T) {
			ctx := contextWithProjects(test.allowedProjects)

			response, err := server.ReadReport(ctx, &reporting.Query{Id: test.reportId})

			assert.NoError(t, err)
			require.NotNil(t, response)
			assert.Equal(t, test.reportId, response.Id)
		})
	}

	failureCases := []struct {
		description     string
		allowedProjects []string
		reportId        string
	}{
		{
			description:     "Projects: user does not have access to any projects an assigned report belongs to",
			allowedProjects: []string{"project3"},
			reportId:        assignedReportId,
		},
		{
			description:     "Projects: user with unassigned access accessing an assigned report",
			allowedProjects: []string{authzConstants.UnassignedProjectID},
			reportId:        assignedReportId,
		},
		{
			description:     "Projects: user without unassigned access accessing an unassigned report",
			allowedProjects: []string{"project1"},
			reportId:        unassignedReportId,
		},
	}

	for _, test := range failureCases {
		t.Run(test.description, func(t *testing.T) {
			ctx := contextWithProjects(test.allowedProjects)

			response, err := server.ReadReport(ctx, &reporting.Query{Id: test.reportId})

			assert.Error(t, err)
			assert.Nil(t, response)
		})
	}
}
