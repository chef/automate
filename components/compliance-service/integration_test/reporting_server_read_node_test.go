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

func TestReadNode(t *testing.T) {
	server := reportingServer.New(&relaxting.ES2Backend{ESUrl: elasticsearchUrl})

	unassignedNodeId := newUUID()
	assignedNodeId := newUUID()

	reports := []*relaxting.ESInSpecReport{
		{
			NodeID:   unassignedNodeId,
			Projects: []string{},
			EndTime:  time.Now(),
		},
		{
			NodeID:   assignedNodeId,
			Projects: []string{"project1", "project2"},
			EndTime:  time.Now(),
		},
	}
	reportIds, err := suite.InsertInspecReports(reports)
	require.NoError(t, err)

	defer suite.DeleteAllDocuments()

	require.Len(t, reportIds, 2)

	successCases := []struct {
		description     string
		allowedProjects []string
		nodeId          string
	}{
		{
			description:     "reporting_server_read_node_test.go => Projects: user has access to all projects accessing an assigned node",
			allowedProjects: []string{authzConstants.AllProjectsExternalID},
			nodeId:          assignedNodeId,
		},
		{
			description:     "reporting_server_read_node_test.go => Projects: user has access to all projects accessing an unassigned node",
			allowedProjects: []string{authzConstants.AllProjectsExternalID},
			nodeId:          unassignedNodeId,
		},
		{
			description:     "reporting_server_read_node_test.go => Projects: user has access to all projects a node belongs to accessing an assigned node",
			allowedProjects: []string{"project1", "project2"},
			nodeId:          assignedNodeId,
		},
		{
			description:     "reporting_server_read_node_test.go => Projects: user has access to some projects a node belongs to accessing an assigned node",
			allowedProjects: []string{"project1", "project3"},
			nodeId:          assignedNodeId,
		},
		{
			description:     "reporting_server_read_node_test.go => Projects: user has access to unassigned nodes accessing an unassigned node",
			allowedProjects: []string{"project1", authzConstants.UnassignedProjectID},
			nodeId:          unassignedNodeId,
		},
	}

	for _, test := range successCases {
		t.Run(test.description, func(t *testing.T) {
			ctx := contextWithProjects(test.allowedProjects)

			response, err := server.ReadNode(ctx, &reporting.Id{Id: test.nodeId})

			assert.NoError(t, err)
			require.NotNil(t, response)
			assert.Equal(t, test.nodeId, response.Id)
		})
	}

	failureCases := []struct {
		description     string
		allowedProjects []string
		nodeId          string
	}{
		{
			description:     "reporting_server_read_node_test.go => Projects: user does not have access to any projects an assigned report belongs to",
			allowedProjects: []string{"project3"},
			nodeId:          assignedNodeId,
		},
		{
			description:     "reporting_server_read_node_test.go => Projects: user with unassigned access accessing an assigned report",
			allowedProjects: []string{authzConstants.UnassignedProjectID},
			nodeId:          assignedNodeId,
		},
		{
			description:     "reporting_server_read_node_test.go => Projects: user without unassigned access accessing an unassigned report",
			allowedProjects: []string{"project1"},
			nodeId:          unassignedNodeId,
		},
	}

	for _, test := range failureCases {
		t.Run(test.description, func(t *testing.T) {
			ctx := contextWithProjects(test.allowedProjects)

			response, err := server.ReadNode(ctx, &reporting.Id{Id: test.nodeId})

			assert.Error(t, err)
			assert.Nil(t, response)
		})
	}
}
