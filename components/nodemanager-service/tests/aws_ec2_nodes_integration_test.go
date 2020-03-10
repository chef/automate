package manager

import (
	"context"
	"sort"
	"testing"

	"github.com/chef/automate/api/interservice/compliance/common"
	"github.com/chef/automate/api/interservice/nodemanager/manager"
	"github.com/chef/automate/api/interservice/nodemanager/nodes"
	"github.com/chef/automate/components/nodemanager-service/tests/mgrtesthelpers"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

var AwsEC2ManagerID = ""

func TestAWSEC2Nodes(t *testing.T) {
	if !mgrtesthelpers.CheckForCreds("aws") {
		t.Log("aws credentials missing; aborting")
		t.FailNow()
	}
	ctx := context.Background()

	mgrConn, err := mgrtesthelpers.GetManagerConn()
	require.NoError(t, err)
	defer mgrConn.Close()

	// setup clients
	mgrClient := manager.NewNodeManagerServiceClient(mgrConn)
	nodesClient := nodes.NewNodesServiceClient(mgrConn)

	// delete all existing aws-ec2 managers, just in case
	err = mgrtesthelpers.DeleteAllManagersByType(ctx, mgrClient, "aws-ec2")
	require.NoError(t, err)

	// create nodemanager
	t.Log("Creating aws-ec2 node manager.")
	mgrID, err := mgrtesthelpers.AddAWSManager(ctx, mgrClient, "aws-ec2")
	require.NoError(t, err)
	require.NotZero(t, len(mgrID.Ids))

	AwsEC2ManagerID = mgrID.GetIds()[0].Id

	t.Log("ensure some nodes were added")
	query := nodes.Query{
		Filters: []*common.Filter{
			{Key: "manager_id", Values: []string{AwsEC2ManagerID}},
		},
	}
	list, err := nodesClient.List(ctx, &query)
	require.NoError(t, err)
	require.NotZero(t, list.GetTotal())

	t.Log("ensure the manager id on the nodes is correct")
	assert.Equal(t, []string{AwsEC2ManagerID}, list.GetNodes()[0].GetManagerIds())

	t.Log("ensure the tags on the nodes were read in and added to db")
	tagsExist := false
	for _, node := range list.GetNodes() {
		if len(node.Tags) > 0 {
			tagsExist = true
			break
		}
	}
	assert.Equal(t, true, tagsExist)

	t.Log("attempt to update node name and host of aws-ec2 node (doesn't work)")
	readNode, err := nodesClient.Read(ctx, &nodes.Id{Id: list.GetNodes()[0].GetId()})
	require.NoError(t, err)
	readNode.Name = "new name"
	readNode.TargetConfig.Host = "localhost"
	readNode.TargetConfig.Port = 22
	_, err = nodesClient.Update(ctx, readNode)
	assert.EqualError(t, err, "rpc error: code = InvalidArgument desc = invalid option. unable to update name of aws-ec2 node")
}

func TestAWSEC2SearchNodeFields(t *testing.T) {
	if !mgrtesthelpers.CheckForCreds("aws") {
		t.Log("aws credentials missing; aborting")
		t.FailNow()
	}
	ctx := context.Background()
	mgrConn, err := mgrtesthelpers.GetManagerConn()
	require.NoError(t, err)
	defer mgrConn.Close()

	// setup clients
	mgrClient := manager.NewNodeManagerServiceClient(mgrConn)

	t.Log("search node fields for list of regions")
	query := manager.FieldQuery{
		NodeManagerId: AwsEC2ManagerID,
		Query: &manager.Query{
			FilterMap: []*common.Filter{},
		},
		Field: "regions",
	}
	fields, err := mgrClient.SearchNodeFields(ctx, &query)
	require.NoError(t, err)
	sort.Strings(fields.GetFields())
	assert.Equal(t, mgrtesthelpers.AWSRegionsList, fields.GetFields())

	t.Log("search node fields for all tags")
	query.Field = "tags"
	fields, err = mgrClient.SearchNodeFields(ctx, &query)
	require.NoError(t, err)
	assert.NotEqual(t, 0, len(fields.GetFields()))
	assert.Equal(t, true, mgrtesthelpers.Contains(fields.GetFields(), "Name"))

	t.Log("search node fields for tags:Name")
	query.Field = "tags:Name"
	fields, err = mgrClient.SearchNodeFields(ctx, &query)
	require.NoError(t, err)
	assert.NotEqual(t, 0, len(fields.GetFields()))

	t.Log("search node fields for all tags, filtered by region")
	query = manager.FieldQuery{
		NodeManagerId: AwsEC2ManagerID,
		Query: &manager.Query{
			FilterMap: []*common.Filter{
				{Key: "region", Values: []string{"us-*"}},
			},
		},
		Field: "tags",
	}
	fields, err = mgrClient.SearchNodeFields(ctx, &query)
	require.NoError(t, err)
	assert.NotEqual(t, 0, len(fields.GetFields()))

	t.Log("search node fields for tags:Name, filtered by excluded regions")
	query = manager.FieldQuery{
		NodeManagerId: AwsEC2ManagerID,
		Query: &manager.Query{
			FilterMap: []*common.Filter{
				{Key: "region", Values: []string{"eu-west*", "ap-south-1"}, Exclude: true},
			},
		},
		Field: "tags:Name",
	}
	fields, err = mgrClient.SearchNodeFields(ctx, &query)
	require.NoError(t, err)
	assert.NotEqual(t, 0, len(fields.GetFields()))
}

func TestAWSEC2SearchNodes(t *testing.T) {
	if !mgrtesthelpers.CheckForCreds("aws") {
		t.Log("aws credentials missing; aborting")
		t.FailNow()
	}
	ctx := context.Background()
	mgrConn, err := mgrtesthelpers.GetManagerConn()
	require.NoError(t, err)
	defer mgrConn.Close()

	// setup clients
	mgrClient := manager.NewNodeManagerServiceClient(mgrConn)

	t.Log("search all nodes for this manager")
	query := manager.NodeQuery{
		NodeManagerId: AwsEC2ManagerID,
		Query: &manager.Query{
			FilterMap: []*common.Filter{},
		},
	}
	nodes, err := mgrClient.SearchNodes(ctx, &query)
	require.NoError(t, err)
	assert.NotEqual(t, int32(0), nodes.GetTotal())

	t.Log("search all nodes for this manager, filtered by regions")
	query = manager.NodeQuery{
		NodeManagerId: AwsEC2ManagerID,
		Query: &manager.Query{
			FilterMap: []*common.Filter{
				{Key: "region", Values: []string{"us-west*"}},
			},
		},
	}
	nodes, err = mgrClient.SearchNodes(ctx, &query)
	require.NoError(t, err)
	assert.NotEqual(t, int32(0), nodes.GetTotal())

	t.Log("search all nodes for this manager, filtered by Name tag")
	query = manager.NodeQuery{
		NodeManagerId: AwsEC2ManagerID,
		Query: &manager.Query{
			FilterMap: []*common.Filter{
				{Key: "Name", Values: []string{"vj*"}},
			},
		},
	}
	nodes, err = mgrClient.SearchNodes(ctx, &query)
	require.NoError(t, err)
	assert.NotEqual(t, int32(0), nodes.GetTotal())
}
