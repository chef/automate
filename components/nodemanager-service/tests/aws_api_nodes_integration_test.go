package manager

import (
	"context"
	"testing"

	"github.com/chef/automate/components/compliance-service/api/common"
	"github.com/chef/automate/components/nodemanager-service/api/manager"
	"github.com/chef/automate/components/nodemanager-service/api/nodes"
	"github.com/chef/automate/components/nodemanager-service/tests/mgrtesthelpers"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestAWSAPINodes(t *testing.T) {
	if !mgrtesthelpers.CheckForCreds("aws") {
		t.Log("aws credentials missing; aborting")
		t.FailNow()
	}
	t.Log("Running AWS-API search nodes test.")
	ctx := context.Background()

	mgrConn, err := mgrtesthelpers.GetManagerConn()
	require.NoError(t, err)
	defer mgrConn.Close()

	t.Log("connection to grpc successful")
	// setup clients
	mgrClient := manager.NewNodeManagerServiceClient(mgrConn)
	nodesClient := nodes.NewNodesServiceClient(mgrConn)

	// delete all existing aws-api managers, just in case
	err = mgrtesthelpers.DeleteAllManagersByType(ctx, mgrClient, "aws-api")
	require.NoError(t, err)

	// create nodemanager
	t.Log("Creating aws-api node manager.")
	mgrID, err := mgrtesthelpers.AddAWSManager(ctx, mgrClient, "aws-api")
	require.NoError(t, err)
	require.NotZero(t, len(mgrID.Ids))

	mgrIDVal := mgrID.GetIds()[0].Id

	t.Log("ensure a node was added for each region")
	query := nodes.Query{
		Filters: []*common.Filter{
			{Key: "manager_id", Values: []string{mgrIDVal}},
		},
	}
	list, err := nodesClient.List(ctx, &query)
	require.NoError(t, err)
	for _, node := range list.GetNodes() {
		assert.Equal(t, true, mgrtesthelpers.Contains(mgrtesthelpers.AWSRegionsList, node.GetName()))
	}

	t.Log("ensure the correct nodes are returned when filtered")
	searchNodesQuery := manager.NodeQuery{
		NodeManagerId: mgrIDVal,
		Query: &manager.Query{
			FilterMap: []*common.Filter{
				{Key: "region", Values: []string{"eu-west-1", "us-east-1"}},
			},
		},
	}
	searchNodesList, err := mgrClient.SearchNodes(ctx, &searchNodesQuery)
	require.NoError(t, err)
	assert.Equal(t, []string{"eu-west-1", "us-east-1"}, searchNodesList.GetNodes())

	t.Log("ensure the correct nodes are returned when filtered with exclusion")
	searchNodesQuery = manager.NodeQuery{
		NodeManagerId: mgrIDVal,
		Query: &manager.Query{
			FilterMap: []*common.Filter{
				{Key: "region", Values: []string{"eu-north-1", "ap-south-1", "eu-west-3", "eu-west-2", "ap-northeast-2", "ap-northeast-1", "sa-east-1", "ca-central-1", "ap-southeast-1", "ap-southeast-2", "eu-central-1", "us-east-2", "us-west-1", "us-west-2"}, Exclude: true},
			},
		},
	}
	searchNodesList, err = mgrClient.SearchNodes(ctx, &searchNodesQuery)
	require.NoError(t, err)
	assert.Equal(t, []string{"eu-west-1", "us-east-1"}, searchNodesList.GetNodes())

	t.Log("ensure the correct nodes are returned when filtered with prefix match")
	searchNodesQuery = manager.NodeQuery{
		NodeManagerId: mgrIDVal,
		Query: &manager.Query{
			FilterMap: []*common.Filter{
				{Key: "region", Values: []string{"eu-west*"}},
			},
		},
	}
	searchNodesList, err = mgrClient.SearchNodes(ctx, &searchNodesQuery)
	require.NoError(t, err)
	assert.Equal(t, []string{"eu-west-3", "eu-west-2", "eu-west-1"}, searchNodesList.GetNodes())

	t.Log("ensure user cannot create query with include and exclude")
	searchNodesQuery = manager.NodeQuery{
		NodeManagerId: mgrIDVal,
		Query: &manager.Query{
			FilterMap: []*common.Filter{
				{Key: "region", Values: []string{"eu-west*"}},
				{Key: "region", Values: []string{"us-east-1"}, Exclude: true},
			},
		},
	}
	searchNodesList, err = mgrClient.SearchNodes(ctx, &searchNodesQuery)
	assert.Equal(t, "rpc error: code = InvalidArgument desc = using include and exclude filters in the same request is unsupported", err.Error())

}
