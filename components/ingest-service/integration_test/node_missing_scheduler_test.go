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

func TestNodesMissingSchedulerBasic(t *testing.T) {
	var (
		ctx      = context.Background()
		req      = new(ingest.MarkNodesMissingRequest)
		expected = new(ingest.MarkNodesMissingResponse)
		now      = time.Now() // The time that we will use to check the timestamp update
		nodes    = []iBackend.Node{
			iBackend.Node{
				NodeInfo: iBackend.NodeInfo{
					EntityUuid:       newUUID(),
					Status:           "success",
					OrganizationName: "awesome_org",
				},
				Exists: true,
			},
		}
	)

	suite.IngestNodes(nodes)
	defer suite.DeleteAllDocuments()

	res, err := suite.JobSchedulerServer.MarkNodesMissing(ctx, req)
	assert.Nil(t, err)
	assert.Equal(t, expected, res)

	// Refresh the node-state index from ES to apply the modifications made
	suite.RefreshIndices(mappings.NodeState.Index)

	// Verify node status and timestamp
	node, err := suite.GetNode(nodes[0].EntityUuid)
	assert.Nil(t, err)
	assert.NotNil(t, node)
	assert.Equal(t, node.Status, "missing", "the node status should be missing")
	assert.True(t, node.Timestamp.After(now), "the node timestamp should be updated")
}

func TestNodesMissingSchedulerMultiNodeUpdate(t *testing.T) {
	var (
		ctx      = context.Background()
		req      = new(ingest.MarkNodesMissingRequest)
		expected = new(ingest.MarkNodesMissingResponse)
		now      = time.Now() // The time that we will use to check the timestamp update
		xN       = 200
		nodes    = xNodeArray(xN, "success")
	)

	suite.IngestNodes(nodes)
	defer suite.DeleteAllDocuments()

	res, err := suite.JobSchedulerServer.MarkNodesMissing(ctx, req)
	assert.Nil(t, err)
	assert.Equal(t, expected, res)

	// Refresh the node-state index from ES to apply the modifications made
	suite.RefreshIndices(mappings.NodeState.Index)

	// Verify that all nodes are marked as missing
	actualNodes, err := suite.GetNodes(xN + 1)
	assert.Nil(t, err)
	assert.Equal(t, xN, len(actualNodes), "wrong number of nodes retrieved")

	for _, node := range actualNodes {
		assert.Equal(t, node.Status, "missing", "the node status should be missing")
		assert.True(t, node.Timestamp.After(now), "the node timestamp should be updated")
	}
}

// XNodeArray returns an Array of X Nodes
func xNodeArray(x int, status string) []iBackend.Node {
	nodes := make([]iBackend.Node, x)
	for i := 0; i < x; i++ {
		nodes[i] = iBackend.Node{
			NodeInfo: iBackend.NodeInfo{
				EntityUuid:       newUUID(),
				Status:           status,
				NodeName:         "node" + string(i),
				OrganizationName: "org1",
				Environment:      "env1",
			},
			Exists: true,
		}
	}
	return nodes
}
