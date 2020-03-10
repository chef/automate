package manager

import (
	"context"
	"testing"

	"github.com/chef/automate/api/interservice/compliance/common"
	"github.com/chef/automate/api/interservice/nodemanager/manager"
	"github.com/chef/automate/api/interservice/nodemanager/nodes"
	"github.com/chef/automate/components/nodemanager-service/tests/mgrtesthelpers"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestAzureAPINodes(t *testing.T) {
	if !mgrtesthelpers.CheckForCreds("azure") {
		t.Log("azure credentials missing; aborting")
		t.FailNow()
	}
	t.Log("Running Azure-API search nodes test.")
	ctx := context.Background()

	mgrConn, err := mgrtesthelpers.GetManagerConn()
	require.NoError(t, err)
	defer mgrConn.Close()

	t.Log("connection to grpc successful")

	// setup clients
	mgrClient := manager.NewNodeManagerServiceClient(mgrConn)
	nodesClient := nodes.NewNodesServiceClient(mgrConn)

	// delete all existing azure-api managers, just in case
	err = mgrtesthelpers.DeleteAllManagersByType(ctx, mgrClient, "azure-api")
	require.NoError(t, err)

	// create nodemanager
	t.Log("Creating azure-api node manager.")
	mgrID, err := mgrtesthelpers.AddAzureManager(ctx, mgrClient, "azure-api")
	require.NoError(t, err)
	require.NotZero(t, len(mgrID.Ids))

	mgrIDVal := mgrID.GetIds()[0].Id

	t.Log("ensure a node was added")
	query := nodes.Query{
		Filters: []*common.Filter{
			{Key: "manager_id", Values: []string{mgrIDVal}},
		},
	}
	list, err := nodesClient.List(ctx, &query)
	require.NoError(t, err)
	assert.Equal(t, int32(1), list.GetTotal())

	t.Log("ensure user cannot update name of node added")
	node, err := nodesClient.Read(ctx, &nodes.Id{Id: list.GetNodes()[0].GetId()})
	require.NoError(t, err)
	node.Name = "updatedName"
	_, err = nodesClient.Update(ctx, node)
	assert.Equal(t, "rpc error: code = InvalidArgument desc = invalid option. unable to update azure-api node", err.Error())
}
