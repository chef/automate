package manager

import (
	"context"
	"testing"

	"github.com/chef/automate/api/interservice/compliance/common"
	"github.com/chef/automate/api/interservice/nodemanager/manager"
	"github.com/chef/automate/api/interservice/nodemanager/nodes"
	"github.com/chef/automate/components/nodemanager-service/tests/mgrtesthelpers"

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

	t.Log("ensure one node was added")
	query := nodes.Query{
		Filters: []*common.Filter{
			{Key: "manager_id", Values: []string{mgrIDVal}},
		},
	}
	list, err := nodesClient.List(ctx, &query)
	require.NoError(t, err)
	require.Equal(t, 1, len(list.Nodes))
}
