package manager

import (
	"context"
	"fmt"
	"testing"
	"time"

	"github.com/chef/automate/components/nodemanager-service/api/manager"
	"github.com/chef/automate/components/nodemanager-service/api/nodes"
	"github.com/chef/automate/components/nodemanager-service/tests/mgrtesthelpers"
	"github.com/golang/protobuf/ptypes"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	authzConstants "github.com/chef/automate/components/authz-service/constants/v2"
	nodesserver "github.com/chef/automate/components/nodemanager-service/api/nodes/server"
	"github.com/chef/automate/lib/grpc/auth_context"
)

func TestListProjectFiltering(t *testing.T) {
	mgrConn, err := mgrtesthelpers.GetManagerConn()
	require.NoError(t, err)
	defer mgrConn.Close()

	db, err := createPGDB()
	require.NoError(t, err)

	// setup clients
	nodeManager := nodesserver.New(db, nil, "")

	timestamp, err := ptypes.TimestampProto(time.Now())

	cases := []struct {
		description     string
		ctx             context.Context
		ingestedNodes   []*manager.NodeMetadata
		expectedNodeIDs []string
	}{
		{
			description: "Two nodes matching on the same project tag",
			ctx:         contextWithProjects([]string{"target_project"}),
			ingestedNodes: []*manager.NodeMetadata{
				{
					Uuid:     "node1",
					Projects: []string{"target_project"},
				},
				{
					Uuid:     "node2",
					Projects: []string{"project8", "target_project"},
				},
			},
			expectedNodeIDs: []string{"node1", "node2"},
		},
		{
			description: "Two nodes matching with two project tags",
			ctx:         contextWithProjects([]string{"target_project_1", "target_project_2"}),
			ingestedNodes: []*manager.NodeMetadata{
				{
					Uuid:     "node1",
					Projects: []string{"target_project_1"},
				},
				{
					Uuid:     "node2",
					Projects: []string{"target_project_2"},
				},
			},
			expectedNodeIDs: []string{"node1", "node2"},
		},
		{
			description: "Two nodes with only one with a matching project",
			ctx:         contextWithProjects([]string{"target_project"}),
			ingestedNodes: []*manager.NodeMetadata{
				{
					Uuid:     "node1",
					Projects: []string{"target_project"},
				},
				{
					Uuid:     "node2",
					Projects: []string{"project8"},
				},
			},
			expectedNodeIDs: []string{"node1"},
		},
		{
			description: "Three nodes with different projects and one missing a project where all match " +
				"because the AllProjectsID is requested",
			ctx: contextWithProjects([]string{authzConstants.AllProjectsExternalID}),
			ingestedNodes: []*manager.NodeMetadata{
				{
					Uuid:     "node1",
					Projects: []string{"project3"},
				},
				{
					Uuid:     "node2",
					Projects: []string{"project8", "project7"},
				},
				{
					Uuid:     "node3",
					Projects: []string{},
				},
			},
			expectedNodeIDs: []string{"node1", "node2", "node3"},
		},
		{
			description: "Two nodes one with a project tag and one with none. Matching one unassigned",
			ctx:         contextWithProjects([]string{authzConstants.UnassignedProjectID}),
			ingestedNodes: []*manager.NodeMetadata{
				{
					Uuid:     "node1",
					Projects: []string{"project9"},
				},
				{
					Uuid:     "node2",
					Projects: []string{},
				},
			},
			expectedNodeIDs: []string{"node2"},
		},
		{
			description: "Two nodes with projects assigned, with unassigned request no matches",
			ctx:         contextWithProjects([]string{authzConstants.UnassignedProjectID}),
			ingestedNodes: []*manager.NodeMetadata{
				{
					Uuid:     "node1",
					Projects: []string{"project9"},
				},
				{
					Uuid:     "node2",
					Projects: []string{"project7"},
				},
			},
			expectedNodeIDs: []string{},
		},
		{
			description: "Two nodes one unassigned and one with a node, with unassigned and macthing " +
				"project request matching both",
			ctx: contextWithProjects([]string{authzConstants.UnassignedProjectID, "target_project"}),
			ingestedNodes: []*manager.NodeMetadata{
				{
					Uuid:     "node1",
					Projects: []string{"target_project"},
				},
				{
					Uuid:     "node2",
					Projects: []string{},
				},
			},
			expectedNodeIDs: []string{"node1", "node2"},
		},
		{
			description: "Two nodes one unassigned and one with a node, with unassigned and macthing " +
				"project request matching both",
			ctx: contextWithProjects([]string{authzConstants.UnassignedProjectID, "target_project"}),
			ingestedNodes: []*manager.NodeMetadata{
				{
					Uuid:     "node1",
					Projects: []string{"target_project"},
				},
				{
					Uuid:     "node2",
					Projects: []string{},
				},
			},
			expectedNodeIDs: []string{"node1", "node2"},
		},
	}

	for _, test := range cases {
		t.Run(fmt.Sprintf("Project filter: %s", test.description),
			func(t *testing.T) {
				// Ingest nodes
				for _, node := range test.ingestedNodes {
					node.LastContact = timestamp
					node.RunData = &nodes.LastContactData{
						Id:      createUUID(),
						EndTime: timestamp,
						Status:  nodes.LastContactData_PASSED,
					}

					err = db.ProcessIncomingNode(node)
					require.NoError(t, err)
				}
				// Delete nodes after the test is complete
				defer func() {
					for _, node := range test.ingestedNodes {
						db.DeleteNode(node.Uuid)
					}
				}()

				projectFilters, _ := filterByProjects(test.ctx)
				t.Logf("projectFilters: %v", projectFilters)
				// Call List to get all ingested nodes with project filtering context.
				nodesResponse, err := nodeManager.List(test.ctx, &nodes.Query{})
				require.NoError(t, err)

				// Get all the node IDs returned.
				actualNodeIDs := []string{}
				for _, node := range nodesResponse.Nodes {
					actualNodeIDs = append(actualNodeIDs, node.Id)
				}

				assert.ElementsMatch(t, test.expectedNodeIDs, actualNodeIDs)
			})
	}
}

func contextWithProjects(projects []string) context.Context {
	ctx := context.Background()
	return auth_context.NewContext(ctx, []string{}, projects, "", "", "")
}

func filterByProjects(ctx context.Context) ([]string, error) {
	projectsFilter, err := auth_context.ProjectsFromIncomingContext(ctx)
	if err != nil {
		return nil, err
	}
	if auth_context.AllProjectsRequested(projectsFilter) {
		return []string{}, nil
	}

	return projectsFilter, nil
}
