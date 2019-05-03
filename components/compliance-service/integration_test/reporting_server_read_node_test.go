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

func TestReadNode(t *testing.T) {
	server := reportingServer.New(&relaxting.ES2Backend{ESUrl: elasticsearchUrl})

	unassignedNodeId := newUUID()
	assignedNodeId := newUUID()

	reports := []*relaxting.ESInSpecReport{
		{
			NodeID:   unassignedNodeId,
			Projects: []string{},
		},
		{
			NodeID:   assignedNodeId,
			Projects: []string{"project1", "project2"},
		},
	}
	reportIds := suite.InsertInspecReports(reports)

	defer suite.DeleteAllDocuments()

	require.Len(t, reportIds, 2)

	successCases := []struct {
		description     string
		allowedProjects []string
		nodeId          string
	}{
		{
			description:     "Projects: user has access to all projects accessing an assigned node",
			allowedProjects: []string{authzConstants.AllProjectsExternalID},
			nodeId:          assignedNodeId,
		},
		{
			description:     "Projects: user has access to all projects accessing an unassigned node",
			allowedProjects: []string{authzConstants.AllProjectsExternalID},
			nodeId:          unassignedNodeId,
		},
		{
			description:     "Projects: user has access to all projects a node belongs to accessing an assigned node",
			allowedProjects: []string{"project1", "project2"},
			nodeId:          assignedNodeId,
		},
		{
			description:     "Projects: user has access to some projects a node belongs to accessing an assigned node",
			allowedProjects: []string{"project1", "project3"},
			nodeId:          assignedNodeId,
		},
		{
			description:     "Projects: user has access to unassigned nodes accessing an unassigned node",
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
			description:     "Projects: user does not have access to any projects an assigned report belongs to",
			allowedProjects: []string{"project3"},
			nodeId:          assignedNodeId,
		},
		{
			description:     "Projects: user with unassigned access accessing an assigned report",
			allowedProjects: []string{authzConstants.UnassignedProjectID},
			nodeId:          assignedNodeId,
		},
		{
			description:     "Projects: user without unassigned access accessing an unassigned report",
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
