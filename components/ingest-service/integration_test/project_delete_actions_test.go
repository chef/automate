package integration_test

import (
	"context"
	"fmt"
	"testing"
	"time"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	iBackend "github.com/chef/automate/components/ingest-service/backend"
	"github.com/chef/automate/components/ingest-service/backend/elastic/mappings"
)

func TestProjectDeleteActions(t *testing.T) {
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
		t.Run(fmt.Sprintf("action match: %s", test.description),
			func(t *testing.T) {
				action := iBackend.InternalChefAction{
					Projects:   test.existingProjectIDs,
					RecordedAt: time.Now(),
				}
				suite.IngestActions([]iBackend.InternalChefAction{action})
				defer suite.DeleteAllDocuments()

				// Send a project rules update event
				esJobID, err := suite.ingest.DeleteActionProjectTag(ctx, test.projectIDToBeDelete)
				require.Nil(t, err)

				waitForJobToComplete(t, esJobID)

				suite.RefreshIndices(fmt.Sprintf("%s-%s", mappings.Actions.Index, "*"))

				// assert the node's project IDs
				actualActions, err := suite.GetActions(100)
				require.Nil(t, err)
				require.Equal(t, 1, len(actualActions), "wrong number of actions retrieved")

				actualAction := actualActions[0]

				require.ElementsMatch(t, test.expectedProjectIDs, actualAction.Projects)
			})
	}
}

func TestProjectDeleteNoActions(t *testing.T) {

	ctx := context.Background()

	// Send a project rules update event
	esJobID, err := suite.ingest.DeleteActionProjectTag(ctx, "project9")
	assert.Nil(t, err)

	waitForJobToComplete(t, esJobID)

	suite.RefreshIndices(fmt.Sprintf("%s-%s", mappings.Actions.Index, "*"))

	// assert the node's project IDs
	actualNodes, err := suite.GetNodes(100)
	assert.Nil(t, err)
	assert.Equal(t, 0, len(actualNodes), "wrong number of actions retrieved")
}
