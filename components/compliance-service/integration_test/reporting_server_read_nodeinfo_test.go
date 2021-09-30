package integration_test

import (
	"fmt"
	"testing"
	"time"

	"github.com/chef/automate/api/interservice/compliance/reporting"
	authzConstants "github.com/chef/automate/components/authz-service/constants"
	reportingServer "github.com/chef/automate/components/compliance-service/api/reporting/server"
	"github.com/chef/automate/components/compliance-service/reporting/relaxting"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestReadNodeInfo(t *testing.T){
	server := reportingServer.New(&relaxting.ES2Backend{ESUrl: elasticsearchUrl})
	reports := []*relaxting.ESInSpecReport{
		{
			Projects: []string{},
			NodeID: "1",
			EndTime: time.Now(),
		},
		{
			Projects: []string{"project1", "project2"},
			NodeID: "2",
			EndTime: time.Now(),
		},
	}
	reportIds, err := suite.InsertInspecReports(reports)
	require.NoError(t, err)

	defer suite.DeleteAllDocuments()

	require.Len(t, reportIds, 2)

	unassignedReportID := reportIds[0]
	assignedReportID := reportIds[1]
	successCases := []struct{
		description string
		allowedProjects []string
		reportID string
		expectdID string
	}{
		{
			description: "reporting_server_read_nodeinfo_test.go => Projects: user has access to all projects accessing an assigned report",
			allowedProjects: []string{authzConstants.AllProjectsExternalID},
			reportID: assignedReportID,
			expectdID: "2",
		},
		{
			description:     "reporting_server_read_nodeinfo_test.go => Projects: user has access to all projects accessing an unassigned report",
			allowedProjects: []string{authzConstants.AllProjectsExternalID},
			reportID:        unassignedReportID,
			expectdID: "1",
		},
		{
			description:     "reporting_server_read_nodeinfo_test.go => Projects: user has access to all projects a report belongs to accessing an assigned report",
			allowedProjects: []string{"project1", "project2"},
			reportID:        assignedReportID,
			expectdID: "2",
		},
		{
			description:     "reporting_server_read_nodeinfo_test.go => Projects: user has access to some projects a report belongs to accessing an assigned report",
			allowedProjects: []string{"project1", "project3"},
			reportID:        assignedReportID,
			expectdID: "2",
		},
		{
			description:     "reporting_server_read_nodeinfo_test.go => Projects: user has access to unassigned reports accessing an unassigned report",
			allowedProjects: []string{"project1", authzConstants.UnassignedProjectID},
			reportID:        assignedReportID,
			expectdID: "2",
		},
	}

	for _, test := range successCases{
		t.Run(test.description, func(t *testing.T) {
			ctx := contextWithProjects(test.allowedProjects)

			response, err := server.ReadNodeHeader(ctx, &reporting.Query{Id: test.reportID})
			fmt.Println(response)
			assert.NoError(t, err)
			require.NotNil(t, response)
			assert.Equal(t, test.expectdID, response.NodeId)
		})
	}


	failureCases := []struct {
		description     string
		allowedProjects []string
		reportID        string
	}{
		{
			description:     "reporting_server_read_nodeinfo_test.go => Projects: user does not have access to any projects an assigned report belongs to",
			allowedProjects: []string{"project3"},
			reportID:        assignedReportID,
		},
		{
			description:     "reporting_server_read_nodeinfo_test.go => Projects: user with unassigned access accessing an assigned report",
			allowedProjects: []string{authzConstants.UnassignedProjectID},
			reportID:        assignedReportID,
		},
		{
			description:     "reporting_server_read_nodeinfo_test.go => Projects: user without unassigned access accessing an unassigned report",
			allowedProjects: []string{"project1"},
			reportID:        unassignedReportID,
		},
	}

	for _, test := range failureCases {
		t.Run(test.description, func(t *testing.T) {
			ctx := contextWithProjects(test.allowedProjects)

			response, err := server.ReadNodeHeader(ctx, &reporting.Query{Id: test.reportID})
			assert.Error(t, err)
			assert.Nil(t, response)
		})
	}
}