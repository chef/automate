package integration_test

import (
	"testing"
	"time"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	"github.com/chef/automate/api/interservice/compliance/reporting"
	authzConstants "github.com/chef/automate/components/authz-service/constants"
	reportingServer "github.com/chef/automate/components/compliance-service/api/reporting/server"
	"github.com/chef/automate/components/compliance-service/reporting/relaxting"
)

func TestReadReport(t *testing.T) {
	server := reportingServer.New(&relaxting.ES2Backend{ESUrl: elasticsearchUrl})
	reports := []*relaxting.ESInSpecReport{
		{
			Projects: []string{},
			EndTime:  time.Now(),
		},
		{
			Projects: []string{"project1", "project2"},
			EndTime:  time.Now(),
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
			description:     "reporting_server_read_report_test.go => Projects: user has access to all projects accessing an assigned report",
			allowedProjects: []string{authzConstants.AllProjectsExternalID},
			reportId:        assignedReportId,
		},
		{
			description:     "reporting_server_read_report_test.go => Projects: user has access to all projects accessing an unassigned report",
			allowedProjects: []string{authzConstants.AllProjectsExternalID},
			reportId:        unassignedReportId,
		},
		{
			description:     "reporting_server_read_report_test.go => Projects: user has access to all projects a report belongs to accessing an assigned report",
			allowedProjects: []string{"project1", "project2"},
			reportId:        assignedReportId,
		},
		{
			description:     "reporting_server_read_report_test.go => Projects: user has access to some projects a report belongs to accessing an assigned report",
			allowedProjects: []string{"project1", "project3"},
			reportId:        assignedReportId,
		},
		{
			description:     "reporting_server_read_report_test.go => Projects: user has access to unassigned reports accessing an unassigned report",
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
			description:     "reporting_server_read_report_test.go => Projects: user does not have access to any projects an assigned report belongs to",
			allowedProjects: []string{"project3"},
			reportId:        assignedReportId,
		},
		{
			description:     "reporting_server_read_report_test.go => Projects: user with unassigned access accessing an assigned report",
			allowedProjects: []string{authzConstants.UnassignedProjectID},
			reportId:        assignedReportId,
		},
		{
			description:     "reporting_server_read_report_test.go => Projects: user without unassigned access accessing an unassigned report",
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
