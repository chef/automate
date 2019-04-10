package integration_test

import (
	"context"
	"testing"

	"github.com/stretchr/testify/assert"

	chef "github.com/chef/automate/api/external/ingest/request"
	iBackend "github.com/chef/automate/components/ingest-service/backend"
)

func TestMarkForDeleteOneNode(t *testing.T) {
	var (
		ctx    = context.Background()
		req    = new(chef.MultipleNodeDeleteRequest)
		nodeID = newUUID()
		nodes  = []iBackend.Node{
			iBackend.Node{
				NodeInfo: iBackend.NodeInfo{
					EntityUuid: nodeID,
				},
				Exists: true,
			},
		}
	)

	suite.IngestNodes(nodes)
	defer suite.DeleteAllDocuments()

	req.NodeIds = []string{nodeID}

	// One node before deleting
	actualNodes, err := suite.GetNodes(1)
	assert.NoError(t, err)
	assert.Equal(t, 1, len(actualNodes))

	_, err = suite.ChefIngestServer.ProcessMultipleNodeDeletes(ctx, req)
	assert.NoError(t, err)

	// zero nodes after deleting
	actualNodes, err = suite.GetNodes(1)
	assert.NoError(t, err)
	assert.Equal(t, 0, len(actualNodes))
}

func TestMarkForDeleteMultipleNodes(t *testing.T) {
	var (
		ctx           = context.Background()
		req           = new(chef.MultipleNodeDeleteRequest)
		nodeIDs       = []string{}
		nodes         = []iBackend.Node{}
		numberOfNodes = 10
	)

	for index := 0; index < numberOfNodes; index++ {
		nodeID := newUUID()
		nodeIDs = append(nodeIDs, nodeID)
		nodes = append(nodes, iBackend.Node{
			NodeInfo: iBackend.NodeInfo{
				EntityUuid: nodeID,
			},
			Exists: true,
		})
	}

	suite.IngestNodes(nodes)
	defer suite.DeleteAllDocuments()

	// One node before deleting
	actualNodes, err := suite.GetNodes(numberOfNodes)
	assert.NoError(t, err)
	assert.Equal(t, numberOfNodes, len(actualNodes))

	req.NodeIds = nodeIDs[:5]
	_, err = suite.ChefIngestServer.ProcessMultipleNodeDeletes(ctx, req)
	assert.NoError(t, err)

	// 5 nodes after deleting
	actualNodes, err = suite.GetNodes(numberOfNodes)
	assert.NoError(t, err)
	assert.Equal(t, 5, len(actualNodes))

	req.NodeIds = nodeIDs[5:]
	_, err = suite.ChefIngestServer.ProcessMultipleNodeDeletes(ctx, req)
	assert.NoError(t, err)

	// zero nodes after deleting
	actualNodes, err = suite.GetNodes(numberOfNodes)
	assert.NoError(t, err)
	assert.Equal(t, 0, len(actualNodes))
}

func TestMarkForDeleteMultipleNodesEmpty(t *testing.T) {
	var (
		ctx           = context.Background()
		req           = new(chef.MultipleNodeDeleteRequest)
		nodeIDs       = []string{}
		nodes         = []iBackend.Node{}
		numberOfNodes = 10
	)

	for index := 0; index < numberOfNodes; index++ {
		nodeID := newUUID()
		nodeIDs = append(nodeIDs, nodeID)
		nodes = append(nodes, iBackend.Node{
			NodeInfo: iBackend.NodeInfo{
				EntityUuid: nodeID,
			},
			Exists: true,
		})
	}

	suite.IngestNodes(nodes)
	defer suite.DeleteAllDocuments()

	// One node before deleting
	actualNodes, err := suite.GetNodes(numberOfNodes)
	assert.NoError(t, err)
	assert.Equal(t, numberOfNodes, len(actualNodes))

	req.NodeIds = []string{}
	_, err = suite.ChefIngestServer.ProcessMultipleNodeDeletes(ctx, req)
	assert.NoError(t, err)

	// no nodes should be removed
	actualNodes, err = suite.GetNodes(numberOfNodes)
	assert.NoError(t, err)
	assert.Equal(t, numberOfNodes, len(actualNodes))
}

func TestMarkForDeleteMultipleNodesFakeNodeID(t *testing.T) {
	var (
		ctx           = context.Background()
		req           = new(chef.MultipleNodeDeleteRequest)
		nodeIDs       = []string{}
		nodes         = []iBackend.Node{}
		numberOfNodes = 10
	)

	for index := 0; index < numberOfNodes; index++ {
		nodeID := newUUID()
		nodeIDs = append(nodeIDs, nodeID)
		nodes = append(nodes, iBackend.Node{
			NodeInfo: iBackend.NodeInfo{
				EntityUuid: nodeID,
			},
			Exists: true,
		})
	}

	suite.IngestNodes(nodes)
	defer suite.DeleteAllDocuments()

	// One node before deleting
	actualNodes, err := suite.GetNodes(numberOfNodes)

	assert.Equal(t, numberOfNodes, len(actualNodes))
	assert.NoError(t, err)

	req.NodeIds = []string{"fake-id"}
	_, err = suite.ChefIngestServer.ProcessMultipleNodeDeletes(ctx, req)
	assert.NoError(t, err)

	// no nodes should be removed
	actualNodes, err = suite.GetNodes(numberOfNodes)
	assert.NoError(t, err)
	assert.Equal(t, numberOfNodes, len(actualNodes))
}
