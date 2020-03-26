package manager

import (
	"context"
	"fmt"
	"testing"
	"time"

	"github.com/chef/automate/api/interservice/nodemanager/manager"
	"github.com/chef/automate/api/interservice/nodemanager/nodes"
	"github.com/golang/protobuf/ptypes"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	authzConstants "github.com/chef/automate/components/authz-service/constants"
	nodesserver "github.com/chef/automate/components/nodemanager-service/server/nodes"
	"github.com/chef/automate/lib/grpc/auth_context"
)

func TestReadProjectFilteringIngestedNodes(t *testing.T) {
	timestamp, err := ptypes.TimestampProto(time.Now())
	require.NoError(t, err)

	db, err := createPGDB()
	require.NoError(t, err)

	nodeManager := nodesserver.New(db, nil, "")

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
	manualNodeID := manualNodeIds[0]

	cases := []struct {
		description     string
		requestProjects []string
		nodeProjects    []string
		isError         bool
	}{
		{
			description:     "Node project matching request's projects",
			requestProjects: []string{"target_project"},
			nodeProjects:    []string{"target_project"},
			isError:         false,
		},
		{
			description:     "Node project not matching request's projects",
			requestProjects: []string{"missed_target_project"},
			nodeProjects:    []string{"target_project"},
			isError:         true,
		},
		{
			description:     "Node has no projects; request's project has unassigned project",
			requestProjects: []string{authzConstants.UnassignedProjectID},
			nodeProjects:    []string{},
			isError:         false,
		},
		{
			description:     "Node has a project assigned; request's project has unassigned and the matching project",
			requestProjects: []string{authzConstants.UnassignedProjectID, "target_project"},
			nodeProjects:    []string{"target_project"},
			isError:         false,
		},
		{
			description:     "Node has a project assigned; request's projects has only the unassigned project",
			requestProjects: []string{authzConstants.UnassignedProjectID},
			nodeProjects:    []string{"target_project"},
			isError:         true,
		},
		{
			description:     "Node is assigned a project; request for all projects",
			requestProjects: []string{authzConstants.AllProjectsExternalID},
			nodeProjects:    []string{"target_project"},
			isError:         false,
		},
		{
			description:     "Node has no projects; requested for all projects",
			requestProjects: []string{authzConstants.AllProjectsExternalID},
			nodeProjects:    []string{},
			isError:         false,
		},
		{
			description:     "Node has no projects; requested projects is empty",
			requestProjects: []string{},
			nodeProjects:    []string{},
			isError:         false,
		},
		{
			description:     "Node has one project not matching any of several requested projects",
			requestProjects: []string{"project3", "project4", "project7", "project6"},
			nodeProjects:    []string{"project9"},
			isError:         true,
		},
		{
			description:     "Node has one project matching one of several requested projects",
			requestProjects: []string{"project3", "project4", "project7", "project6"},
			nodeProjects:    []string{"project7"},
			isError:         false,
		},
		{
			description:     "Node with several projects where one matches a single requested project",
			requestProjects: []string{"project3"},
			nodeProjects:    []string{"project3", "project4", "project7", "project6"},
			isError:         false,
		},
		{
			description:     "Node with several projects where one matches one of several requested projects",
			requestProjects: []string{"project3", "project10", "project12", "project13"},
			nodeProjects:    []string{"project3", "project4", "project7", "project6"},
			isError:         false,
		},
		{
			description:     "Node with several projects that do not matche several requested projects",
			requestProjects: []string{"project14", "project10", "project12", "project13"},
			nodeProjects:    []string{"project3", "project4", "project7", "project6"},
			isError:         true,
		},
		{
			description:     "Node with several projects where two match two of several requested projects",
			requestProjects: []string{"project3", "project10", "project12", "project13"},
			nodeProjects:    []string{"project3", "project10", "project7", "project6"},
			isError:         false,
		},
	}

	for _, test := range cases {
		t.Run(fmt.Sprintf("Project filter: %s", test.description),
			func(t *testing.T) {

				// Create the context with projects added
				ctx := contextWithProjects(test.requestProjects)

				// Create Ingest node
				node := &manager.NodeMetadata{
					Uuid:        "node1",
					Projects:    test.nodeProjects,
					LastContact: timestamp,
					RunData: &nodes.LastContactData{
						Id:      createUUID(),
						EndTime: timestamp,
						Status:  nodes.LastContactData_PASSED,
					},
				}

				// ingest node
				err = db.ProcessIncomingNode(node)
				require.NoError(t, err)

				// Delete created node after the test is complete
				defer db.DeleteNode(node.Uuid)

				// Call Read to get the ingested node with project filtering context.
				nodeResponse, err := nodeManager.Read(ctx, &nodes.Id{
					Id: node.Uuid,
				})

				if test.isError {
					assert.Error(t, err)
				} else {
					require.NoError(t, err)
					assert.Equal(t, node.Uuid, nodeResponse.Id)
				}

				// Test that we can read the manually added node for all cases.
				manualNodeResponse, err := nodeManager.Read(ctx, &nodes.Id{Id: manualNodeID})
				require.NoError(t, err)
				assert.Equal(t, manualNodeID, manualNodeResponse.Id)
			})
	}
}

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
				actualManualNodeIDs := []string{}
				for _, node := range nodesResponse.Nodes {
					if node.Manager == "" {
						actualNodeIDs = append(actualNodeIDs, node.Id)
					} else {
						actualManualNodeIDs = append(actualManualNodeIDs, node.Id)
					}
				}

				for _, nodeID := range manualNodeIds {
					assert.Contains(t, actualManualNodeIDs, nodeID)
				}

				assert.ElementsMatch(t, actualNodeIDs, test.expectedIngestedNodeIDs)
			})
	}
}

func contextWithProjects(projects []string) context.Context {
	ctx := context.Background()
	return auth_context.NewContext(ctx, []string{}, projects, "", "", "")
}
