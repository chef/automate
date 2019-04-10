//
//  Author:: Salim Afiune <afiune@chef.io>
//  Copyright:: Copyright 2017, Chef Software Inc.
//

package integration_test

import (
	"context"
	"testing"
	"time"

	"github.com/stretchr/testify/assert"

	"github.com/chef/automate/api/interservice/ingest"
	iBackend "github.com/chef/automate/components/ingest-service/backend"
	"github.com/chef/automate/components/ingest-service/backend/elastic/mappings"
)

func TestMissingNodesForDeletionSchedulerBasic(t *testing.T) {
	var (
		ctx      = context.Background()
		req      = new(ingest.MarkMissingNodesForDeletionRequest)
		expected = new(ingest.MarkMissingNodesForDeletionResponse)
		now      = time.Now() // The time that we will use to check the timestamp update
		nodes    = []iBackend.Node{
			iBackend.Node{
				NodeInfo: iBackend.NodeInfo{
					EntityUuid:       newUUID(),
					Status:           "missing",
					OrganizationName: "awesome_org",
				},
				Exists: true,
			},
		}
	)

	suite.IngestNodes(nodes)
	defer suite.DeleteAllDocuments()

	res, err := suite.JobSchedulerServer.MarkMissingNodesForDeletion(ctx, req)
	assert.Nil(t, err)
	assert.Equal(t, expected, res)

	// Refresh the node-state index from ES to apply the modifications made
	suite.RefreshIndices(mappings.NodeState.Index)

	// Verify that the node does NOT exist anymore
	_, err = suite.GetNode(nodes[0].EntityUuid)
	assert.NotNil(t, err, "trying to retrieve the deleted node should throw an error")
	assert.Contains(t, err.Error(), "type=NodeNotFound")

	// Use a custom function to get non existing nodes and verify the timestamp
	nonExistingNodes, err := suite.GetNonExistingNodes(3)
	assert.Nil(t, err)
	assert.Equal(t, 1, len(nonExistingNodes), "one non existing node should be retrieved")
	assert.True(t, nonExistingNodes[0].Timestamp.After(now), "the non existing node timestamp should be updated")
}

func TestMissingNodesForDeletionSchedulerMultiNodeUpdate(t *testing.T) {
	var (
		ctx      = context.Background()
		req      = new(ingest.MarkMissingNodesForDeletionRequest)
		expected = new(ingest.MarkMissingNodesForDeletionResponse)
		now      = time.Now() // The time that we will use to check the timestamp update
		xN       = 200
		nodes    = xNodeArray(xN, "missing")
	)

	suite.IngestNodes(nodes)
	defer suite.DeleteAllDocuments()

	res, err := suite.JobSchedulerServer.MarkMissingNodesForDeletion(ctx, req)
	assert.Nil(t, err)
	assert.Equal(t, expected, res)

	// Refresh the node-state index from ES to apply the modifications made
	suite.RefreshIndices(mappings.NodeState.Index)

	// Verify that all nodes are marked for deletion (that is exists=false), which means
	// that there should be zero nodes retrieved from this query
	actualNodes, err := suite.GetNodes(xN + 1)
	assert.Nil(t, err)
	assert.Equal(t, 0, len(actualNodes), "zero nodes should be retrieved")

	// Use a custom function to get non existing nodes and verify the timestamp
	nonExistingNodes, err := suite.GetNonExistingNodes(xN + 1)
	assert.Nil(t, err)
	assert.Equal(t, xN, len(nonExistingNodes), "200 non existing nodes should be retrieved")
	for _, nonExistingNode := range nonExistingNodes {
		assert.True(t, nonExistingNode.Timestamp.After(now), "the non existing node timestamp should be updated")
	}
}

func TestMissingNodesForDeletionSchedulerModifyJobSettings(t *testing.T) {
	var (
		ctx            = context.Background()
		expectedRes    = new(ingest.ConfigureMissingNodesForDeletionSchedulerResponse)
		statusReq      = new(ingest.JobSchedulerStatusRequest)
		everySetting   = "50m"
		jobSettingsReq = &ingest.JobSettings{Every: everySetting}
	)

	res, err := suite.JobSchedulerServer.ConfigureMissingNodesForDeletionScheduler(ctx, jobSettingsReq)
	assert.Nil(t, err)
	assert.Equal(t, expectedRes, res)

	// wait for the job to be configured
	waitForModificationsToApply()

	// Check default value
	schedulerStatus, err := suite.JobSchedulerServer.GetStatusJobScheduler(ctx, statusReq)
	assert.Nil(t, err)
	assert.Equal(t, true, schedulerStatus.Running, "job scheduler should be running by default")
	// As well as all its jobs
	for _, job := range schedulerStatus.Jobs {
		if job.Name == "missing_nodes_for_deletion" {
			assert.Equal(t, everySetting, job.Every, "job should not be running")
		}
	}
}
