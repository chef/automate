package manager

import (
	"context"
	"fmt"
	"testing"
	"time"

	"github.com/chef/automate/components/nodemanager-service/api/manager"
	"github.com/chef/automate/components/nodemanager-service/api/nodes"
	"github.com/golang/protobuf/ptypes"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	authzConstants "github.com/chef/automate/components/authz-service/constants/v2"
	nodesserver "github.com/chef/automate/components/nodemanager-service/api/nodes/server"
	"github.com/chef/automate/lib/grpc/auth_context"
)

// All manually added nodes should be returned because they are not included in project filtering.
func TestListProjectFilteringIngestedNodes(t *testing.T) {
	db, err := createPGDB()
	require.NoError(t, err)

	nodeManager := nodesserver.New(db, nil, "")

	timestamp, err := ptypes.TimestampProto(time.Now())

	// Adding a manual node
	mgr1 := manager.NodeManager{Name: "mgr1", Type: "aws-ec2"}
	mgrID1, err := db.AddNodeManager(&mgr1, "11111111")
	require.NoError(t, err)
	defer db.DeleteNodeManager(mgrID1)

	node1 := manager.ManagerNode{Id: "i-1111111", Region: "us-west-2", Host: "Node1"}

	instances := []*manager.ManagerNode{&node1}
	manualNodeIds := db.AddManagerNodesToDB(instances, mgrID1, "242403433", []*manager.CredentialsByTags{}, "aws-ec2")
	require.NoError(t, err)
	defer func() {
		for _, node := range manualNodeIds {
			db.DeleteNode(node)
		}
	}()

	assert.Equal(t, 1, len(manualNodeIds))

	cases := []struct {
		description             string
		ctx                     context.Context
		ingestedNodes           []*manager.NodeMetadata
		expectedIngestedNodeIDs []string
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
			expectedIngestedNodeIDs: []string{"node1", "node2"},
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
			expectedIngestedNodeIDs: []string{"node1", "node2"},
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
			expectedIngestedNodeIDs: []string{"node1"},
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
			expectedIngestedNodeIDs: []string{"node1", "node2", "node3"},
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
			expectedIngestedNodeIDs: []string{"node2"},
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
			expectedIngestedNodeIDs: []string{},
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
			expectedIngestedNodeIDs: []string{"node1", "node2"},
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
			expectedIngestedNodeIDs: []string{"node1", "node2"},
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

				// Call List to get all ingested nodes with project filtering context.
				nodesResponse, err := nodeManager.List(test.ctx, &nodes.Query{})
				require.NoError(t, err)

				// Get all the node IDs returned.
				actualNodeIDs := []string{}
				for _, node := range nodesResponse.Nodes {
					actualNodeIDs = append(actualNodeIDs, node.Id)
				}

				assert.ElementsMatch(t, append(test.expectedIngestedNodeIDs, manualNodeIds...), actualNodeIDs)
			})
	}
}

func contextWithProjects(projects []string) context.Context {
	ctx := context.Background()
	return auth_context.NewContext(ctx, []string{}, projects, "", "", "")
}
