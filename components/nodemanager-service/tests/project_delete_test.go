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
)

func TestProjectDelete(t *testing.T) {
	ctx := context.Background()
	db, err := createPGDB()
	require.NoError(t, err)

	timestamp, err := ptypes.TimestampProto(time.Now())
	require.NoError(t, err)

	cases := []struct {
		description        string
		originalProjectIDs []string
		projectIDToBeDeleted string
		expectedProjectIDs []string
	}{
		{
			description:         "Deleting the last project tag",
			originalProjectIDs:  []string{"target_project"},
			projectIDToBeDeleted: "target_project",
			expectedProjectIDs:  []string{},
		},
		{
			description:         "Deleting a project that does not exist",
			originalProjectIDs:  []string{},
			projectIDToBeDeleted: "target_project",
			expectedProjectIDs:  []string{},
		},
		{
			description:         "Deleting a project that does not exist with multiple projects on node",
			originalProjectIDs:  []string{"project3", "project4", "project9"},
			projectIDToBeDeleted: "target_project",
			expectedProjectIDs:  []string{"project3", "project4", "project9"},
		},
		{
			description:         "Deleting a project on a node with multiple projects",
			originalProjectIDs:  []string{"project3", "target_project", "project9"},
			projectIDToBeDeleted: "target_project",
			expectedProjectIDs:  []string{"project3", "project9"},
		},
	}

	for _, test := range cases {
		t.Run(fmt.Sprintf("project delete: %s", test.description),
			func(t *testing.T) {
				nodeID := createUUID()
				originalNode := &manager.NodeMetadata{
					Uuid:        nodeID,
					Projects:    test.originalProjectIDs,
					LastContact: timestamp,
					RunData: &nodes.LastContactData{
						Id:      createUUID(),
						EndTime: timestamp,
						Status:  nodes.LastContactData_PASSED,
					},
				}

				// Ingest node
				err = db.ProcessIncomingNode(originalNode)
				require.NoError(t, err)
				defer db.DeleteNode(nodeID)

				// Update project
				jobIDs, err := db.DeleteProjectTag(ctx, test.projectIDToBeDeleted)
				require.NoError(t, err)
				require.Equal(t, 1, len(jobIDs))

				waitForJobToComplete(ctx, t, db, jobIDs[0])

				// Get Update node
				processedNode, err := db.GetNode(ctx, nodeID)
				require.NoError(t, err)

				assert.ElementsMatch(t, test.expectedProjectIDs, processedNode.Projects)
			})
	}
}
