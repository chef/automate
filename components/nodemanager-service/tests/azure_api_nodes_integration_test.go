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

var AzureAPIManagerID = ""

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

	AzureAPIManagerID = mgrID.GetIds()[0].Id

	t.Log("ensure a node was added")
	query := nodes.Query{
		Filters: []*common.Filter{
			{Key: "manager_id", Values: []string{AzureAPIManagerID}},
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

func TestAzureApiSearchNodeFields(t *testing.T) {
	if !mgrtesthelpers.CheckForCreds("azure") {
		t.Log("azure credentials missing; aborting")
		t.FailNow()
	}
	t.Log("Running Azure-API search node fields test.")
	ctx := context.Background()

	mgrConn, err := mgrtesthelpers.GetManagerConn()
	require.NoError(t, err)
	defer mgrConn.Close()

	// setup clients
	mgrClient := manager.NewNodeManagerServiceClient(mgrConn)

	t.Log("search node fields for list of subscriptions")
	query := manager.FieldQuery{
		NodeManagerId: AzureAPIManagerID,
		Query: &manager.Query{
			FilterMap: []*common.Filter{},
		},
		Field: "subscriptions",
	}

	fields, err := mgrClient.SearchNodeFields(ctx, &query)
	require.NoError(t, err)
	require.NotEqual(t, 0, len(fields.GetFields()))

	t.Log("search node fields for list of names")
	query.Field = "names"
	fields, err = mgrClient.SearchNodeFields(ctx, &query)
	require.NoError(t, err)
	require.NotEqual(t, 0, len(fields.GetFields()))

	t.Log("search node fields for list of tags")
	query.Field = "tags"
	fields, err = mgrClient.SearchNodeFields(ctx, &query)
	require.NoError(t, err)
	require.NotEqual(t, 0, len(fields.GetFields()))
	t.Logf("fields found are %v", fields)
	require.Equal(t, mgrtesthelpers.Contains(fields.GetFields(), "name"), true)

	t.Log("search node fields for list of values for names")
	query.Field = "tags:name"
	fields, err = mgrClient.SearchNodeFields(ctx, &query)
	require.NoError(t, err)
	require.NotEqual(t, 0, len(fields.GetFields()))
}

func TestAzureApiSearchNodes(t *testing.T) {
	if !mgrtesthelpers.CheckForCreds("azure") {
		t.Log("azure credentials missing; aborting")
		t.FailNow()
	}
	t.Log("Running Azure-API search nodes test.")
	ctx := context.Background()

	mgrConn, err := mgrtesthelpers.GetManagerConn()
	require.NoError(t, err)
	defer mgrConn.Close()

	// setup clients
	mgrClient := manager.NewNodeManagerServiceClient(mgrConn)

	t.Log("search all nodes for this manager")
	query := manager.NodeQuery{
		NodeManagerId: AzureAPIManagerID,
		Query: &manager.Query{
			FilterMap: []*common.Filter{},
		},
	}
	nodes, err := mgrClient.SearchNodes(ctx, &query)
	require.NoError(t, err)
	require.NotEqual(t, 0, len(nodes.GetNodes()))

	t.Log("search all nodes for this manager, filtered by name")
	query = manager.NodeQuery{
		NodeManagerId: AzureAPIManagerID,
		Query: &manager.Query{
			FilterMap: []*common.Filter{
				{Key: "name", Values: []string{"Inspec"}},
			},
		},
	}
	nodes, err = mgrClient.SearchNodes(ctx, &query)
	require.NoError(t, err)
	require.NotEqual(t, 0, len(nodes.GetNodes()))

	t.Log("search all nodes for this manager, filtered by region")
	query = manager.NodeQuery{
		NodeManagerId: AzureAPIManagerID,
		Query: &manager.Query{
			FilterMap: []*common.Filter{
				{Key: "name", Values: []string{"Inspec"}, Exclude: true},
			},
		},
	}
	nodes, err = mgrClient.SearchNodes(ctx, &query)
	require.NoError(t, err)
	require.Equal(t, 0, len(nodes.GetNodes()))
}
