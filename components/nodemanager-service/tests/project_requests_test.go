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

	"github.com/chef/automate/lib/grpc/auth_context"
)

func TestProjectRequests(t *testing.T) {

	mgrConn, err := mgrtesthelpers.GetManagerConn()
	require.NoError(t, err)
	defer mgrConn.Close()

	db, err := createPGDB()
	require.NoError(t, err)

	// setup clients
	mgrClient := nodes.NewNodesServiceClient(mgrConn)
	assert.NotNil(t, mgrClient)

	timestamp, err := ptypes.TimestampProto(time.Now())

	cases := []struct {
		description     string
		ctx             context.Context
		ingestedNodes   []*manager.NodeMetadata
		expectedNodeIDs []string
	}{
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
				defer func() {
					for _, node := range test.ingestedNodes {
						db.DeleteNode(node.Uuid)
					}
				}()

				mgrs, err := mgrClient.List(test.ctx, &nodes.Query{})
				require.NoError(t, err)

				actualNodeIDs := []string{}
				for _, node := range mgrs.Nodes {
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
