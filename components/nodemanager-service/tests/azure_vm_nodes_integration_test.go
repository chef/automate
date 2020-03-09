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

var AzureVMManagerID = ""

func TestAzureVMNodes(t *testing.T) {
	if !mgrtesthelpers.CheckForCreds("azure") {
		t.Log("azure credentials missing; aborting")
		t.FailNow()
	}
	t.Log("Running Azure-VM nodes test.")
	ctx := context.Background()

	mgrConn, err := mgrtesthelpers.GetManagerConn()
	require.NoError(t, err)
	defer mgrConn.Close()

	// setup clients
	mgrClient := manager.NewNodeManagerServiceClient(mgrConn)
	nodesClient := nodes.NewNodesServiceClient(mgrConn)

	// delete all existing azure-vm managers, just in case
	err = mgrtesthelpers.DeleteAllManagersByType(ctx, mgrClient, "azure-vm")
	require.NoError(t, err)

	// create nodemanager
	t.Log("Creating azure-vm node manager.")
	mgrID, err := mgrtesthelpers.AddAzureManager(ctx, mgrClient, "azure-vm")
	require.NoError(t, err)
	require.NotZero(t, len(mgrID.Ids))

	AzureVMManagerID = mgrID.GetIds()[0].Id

	t.Log("ensure some node were added")
	query := nodes.Query{
		Filters: []*common.Filter{
			{Key: "manager_id", Values: []string{AzureVMManagerID}},
		},
	}
	list, err := nodesClient.List(ctx, &query)
	require.NoError(t, err)
	assert.Equal(t, true, list.GetTotal() > 0)

	t.Log("ensure the manager id on the nodes is correct")
	assert.Equal(t, []string{AzureVMManagerID}, list.GetNodes()[0].GetManagerIds())

	t.Log("ensure the tags on the nodes were read in and added to db")
	tagsExist := false
	for _, node := range list.GetNodes() {
		if len(node.Tags) > 0 {
			tagsExist = true
			break
		}
	}
	assert.Equal(t, true, tagsExist)
}

func TestAzureVMSearchNodeFields(t *testing.T) {
	if !mgrtesthelpers.CheckForCreds("azure") {
		t.Log("azure credentials missing; aborting")
		t.FailNow()
	}
	t.Log("Running Azure-VM search node fields test.")
	ctx := context.Background()

	mgrConn, err := mgrtesthelpers.GetManagerConn()
	require.NoError(t, err)
	defer mgrConn.Close()

	// setup clients
	mgrClient := manager.NewNodeManagerServiceClient(mgrConn)

	t.Log("search node fields for list of subscriptions")
	query := manager.FieldQuery{
		NodeManagerId: AzureVMManagerID,
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

	t.Log("search node fields for list of regions")
	query.Field = "regions"
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

	t.Log("search node fields for list of values for tags:Test")
	query.Field = "tags:Test"
	fields, err = mgrClient.SearchNodeFields(ctx, &query)
	require.NoError(t, err)
	require.NotEqual(t, 0, len(fields.GetFields()))

	t.Log("search node fields for list of values for names")
	query.Field = "tags:name"
	fields, err = mgrClient.SearchNodeFields(ctx, &query)
	require.NoError(t, err)
	require.NotEqual(t, 0, len(fields.GetFields()))

	t.Log("search node fields for tags, filtered by region")
	query = manager.FieldQuery{
		NodeManagerId: AzureVMManagerID,
		Query: &manager.Query{
			FilterMap: []*common.Filter{
				{Key: "region", Values: []string{"us*"}},
			},
		},
		Field: "tags",
	}
	fields, err = mgrClient.SearchNodeFields(ctx, &query)
	require.NoError(t, err)
	require.NotEqual(t, 0, len(fields.GetFields()))
}

func TestAzureVMSearchNodes(t *testing.T) {
	if !mgrtesthelpers.CheckForCreds("azure") {
		t.Log("azure credentials missing; aborting")
		t.FailNow()
	}
	t.Log("Running Azure-VM search nodes test.")
	ctx := context.Background()

	mgrConn, err := mgrtesthelpers.GetManagerConn()
	require.NoError(t, err)
	defer mgrConn.Close()

	// setup clients
	mgrClient := manager.NewNodeManagerServiceClient(mgrConn)

	t.Log("search all nodes for this manager")
	query := manager.NodeQuery{
		NodeManagerId: AzureVMManagerID,
		Query: &manager.Query{
			FilterMap: []*common.Filter{},
		},
	}
	nodes, err := mgrClient.SearchNodes(ctx, &query)
	require.NoError(t, err)
	require.NotEqual(t, 0, len(nodes.GetNodes()))

	t.Log("search all nodes for this manager, filtered by name")
	query = manager.NodeQuery{
		NodeManagerId: AzureVMManagerID,
		Query: &manager.Query{
			FilterMap: []*common.Filter{
				{Key: "name", Values: []string{"vj"}},
			},
		},
	}
	nodes, err = mgrClient.SearchNodes(ctx, &query)
	require.NoError(t, err)
	require.NotEqual(t, 0, len(nodes.GetNodes()))

	t.Log("search all nodes for this manager, filtered by region")
	query = manager.NodeQuery{
		NodeManagerId: AzureVMManagerID,
		Query: &manager.Query{
			FilterMap: []*common.Filter{
				{Key: "name", Values: []string{"useast"}, Exclude: true},
			},
		},
	}
	nodes, err = mgrClient.SearchNodes(ctx, &query)
	require.NoError(t, err)
	require.NotEqual(t, 0, len(nodes.GetNodes()))

	t.Log("search all nodes for this manager, filtered by tags")
	query = manager.NodeQuery{
		NodeManagerId: AzureVMManagerID,
		Query: &manager.Query{
			FilterMap: []*common.Filter{
				{Key: "Test", Values: []string{"VJ"}},
			},
		},
	}
	nodes, err = mgrClient.SearchNodes(ctx, &query)
	require.NoError(t, err)
	require.NotEqual(t, 0, len(nodes.GetNodes()))
}
