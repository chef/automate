package integration_test

import (
	"context"
	"fmt"
	"testing"
	"time"

	iBackend "github.com/chef/automate/components/ingest-service/backend"
	"github.com/chef/automate/components/ingest-service/backend/elastic/mappings"
	"github.com/stretchr/testify/assert"
)

func TestNodeProjectDelete(t *testing.T) {
	var (
		ctx = context.Background()
	)

	cases := []struct {
		description         string
		existingProjectIDs  []string
		projectIDToBeDelete string
		expectedProjectIDs  []string
	}{
		{
			description:         "Deleting the last project tag",
			existingProjectIDs:  []string{"target_project"},
			projectIDToBeDelete: "target_project",
			expectedProjectIDs:  []string{},
		},
		{
			description:         "Deleting a project that does not exist",
			existingProjectIDs:  []string{},
			projectIDToBeDelete: "target_project",
			expectedProjectIDs:  []string{},
		},
		{
			description:         "Deleting a project on a node with multiple projects",
			existingProjectIDs:  []string{"project3", "target_project", "project9"},
			projectIDToBeDelete: "target_project",
			expectedProjectIDs:  []string{"project3", "project9"},
		},
	}

	for _, test := range cases {
		t.Run(fmt.Sprintf("node match: %s", test.description),
			func(t *testing.T) {
				node := iBackend.Node{
					NodeInfo: iBackend.NodeInfo{
						EntityUuid:       newUUID(),
						NodeName:         "node_1",
						OrganizationName: "org1",
						Environment:      "env1",
					},
					Projects: test.existingProjectIDs,
					Exists:   true,
				}

				suite.IngestNodes([]iBackend.Node{node})
				defer suite.DeleteAllDocuments()

				// start the project delete
				esJobID, err := suite.ingest.DeleteNodeProjectTag(ctx, test.projectIDToBeDelete)
				assert.Nil(t, err)

				waitForJobToComplete(t, esJobID)

				suite.RefreshIndices(mappings.NodeState.Index)

				// assert the node's project IDs
				actualNodes, err := suite.GetNodes(100)
				assert.Nil(t, err)
				assert.Equal(t, 1, len(actualNodes), "wrong number of nodes retrieved")

				actualNode := actualNodes[0]

				assert.ElementsMatch(t, test.expectedProjectIDs, actualNode.Projects)
			})
	}
}

func TestNodeProjectDeleteNoNodes(t *testing.T) {

	ctx := context.Background()

	// Send a project rules update event
	esJobID, err := suite.ingest.DeleteNodeProjectTag(ctx, "project9")
	assert.Nil(t, err)

	waitForJobToComplete(t, esJobID)

	suite.RefreshIndices(mappings.NodeState.Index)

	// assert the node's project IDs
	actualNodes, err := suite.GetNodes(100)
	assert.Nil(t, err)
	assert.Equal(t, 0, len(actualNodes), "wrong number of nodes retrieved")
}

func waitForJobToComplete(t *testing.T, esJobID string) {
	jobStatus, err := suite.ingest.JobStatus(context.Background(), esJobID)
	assert.Nil(t, err)
	for !jobStatus.Completed {
		time.Sleep(time.Millisecond * 5)
		jobStatus, err = suite.ingest.JobStatus(context.Background(), esJobID)
		assert.Nil(t, err)
		if err != nil {
			assert.FailNow(t, "testing job status")
		}
	}
}
