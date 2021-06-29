package manager

import (
	"context"
	"os"
	"testing"

	"github.com/chef/automate/api/interservice/compliance/common"
	"github.com/chef/automate/api/interservice/nodemanager/manager"
	"github.com/chef/automate/api/interservice/nodemanager/nodes"
	"github.com/chef/automate/components/nodemanager-service/mgrtypes"
	"github.com/chef/automate/components/nodemanager-service/tests/mgrtesthelpers"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestNodemanagers(t *testing.T) {
	if !mgrtesthelpers.CheckForCreds("aws") {
		t.Log("aws credentials missing; aborting")
		t.FailNow()
	}
	if !mgrtesthelpers.CheckForCreds("azure") {
		t.Log("azure credentials missing; aborting")
		t.FailNow()
	}
	ctx := context.Background()

	mgrConn, err := mgrtesthelpers.GetManagerConn()
	require.NoError(t, err)
	defer mgrConn.Close()

	// setup clients
	mgrClient := manager.NewNodeManagerServiceClient(mgrConn)

	t.Log("delete all managers")
	mgrtesthelpers.DeleteAllManagersByType(ctx, mgrClient, "")

	t.Log("add aws and azure managers")
	mgrtesthelpers.AddAWSManager(ctx, mgrClient, "aws-api")
	mgrtesthelpers.AddAWSManager(ctx, mgrClient, "aws-ec2")
	mgrtesthelpers.AddAzureManager(ctx, mgrClient, "azure-api")

	t.Log("try to add a manager with a bad secret")
	awsMgr := manager.NodeManager{
		Name: "my test aws ec2 mgr",
		Type: "aws-ec2",
		CredentialData: []*common.Kv{
			{Key: "AWS_ACCESS_KEY_ID", Value: os.Getenv("AWS_ACCESS_KEY_ID")},
			{Key: "AWS_SECRET_ACCESS_KEY", Value: "lala"}},
	}
	_, err = mgrClient.Create(ctx, &awsMgr)
	assert.Contains(t, err.Error(), "InvalidArgument desc")

	t.Log("try to add a manager with an invalid type")
	mgr := manager.NodeManager{
		Name: "my test mgr",
		Type: "mario",
	}
	_, err = mgrClient.Create(ctx, &mgr)
	assert.Contains(t, err.Error(), "InvalidArgument desc = valid types for node manager instance are [aws-ec2 aws-api azure-api aws azure azure-vm gcp gcp-api]")

	t.Log("list nodemanagers")
	mgrs, err := mgrClient.List(ctx, &manager.Query{})
	require.NoError(t, err)
	assert.NotEqual(t, 0, mgrs.GetTotal())

	t.Log("list nodemanagers, filtered by type")
	mgrsFiltered, err := mgrClient.List(ctx, &manager.Query{
		FilterMap: []*common.Filter{
			{Key: "manager_type", Values: []string{"aws-ec2"}},
		},
	})
	require.NoError(t, err)
	assert.Equal(t, int32(1), mgrsFiltered.GetTotal())
	assert.Equal(t, "aws-ec2", mgrsFiltered.GetManagers()[0].GetType())

	t.Log("list nodemanagers, sorted by type")
	mgrsSortedType, err := mgrClient.List(ctx, &manager.Query{
		Sort: "type",
	})
	require.NoError(t, err)
	assert.Equal(t, "automate", mgrsSortedType.GetManagers()[0].GetType())
	assert.Equal(t, "aws-api", mgrsSortedType.GetManagers()[1].GetType())
	assert.Equal(t, "aws-ec2", mgrsSortedType.GetManagers()[2].GetType())
	assert.Equal(t, "azure-api", mgrsSortedType.GetManagers()[3].GetType())

	t.Log("list nodemanagers, sorted by status")
	mgrsSortedStatus, err := mgrClient.List(ctx, &manager.Query{
		Sort: "status",
	})
	require.NoError(t, err)
	assert.Equal(t, "", mgrsSortedStatus.GetManagers()[0].GetStatus())
	assert.Equal(t, "reachable", mgrsSortedStatus.GetManagers()[1].GetStatus())
	assert.Equal(t, "reachable", mgrsSortedStatus.GetManagers()[2].GetStatus())

	t.Log("list nodemanagers, sorted by date_added")
	mgrsSortedDate, err := mgrClient.List(ctx, &manager.Query{
		Sort:  "date_added",
		Order: 1,
	})
	require.NoError(t, err)
	assert.Equal(t, "Automate", mgrsSortedDate.GetManagers()[3].GetName())
	assert.Equal(t, "my test aws-api mgr", mgrsSortedDate.GetManagers()[2].GetName())
	assert.Equal(t, "my test aws-ec2 mgr", mgrsSortedDate.GetManagers()[1].GetName())
	assert.Equal(t, "my test azure-api mgr", mgrsSortedDate.GetManagers()[0].GetName())

	t.Log("updating nodemanager does not create new credential, only updates it")
	mgrRead, err := mgrClient.Read(ctx, &manager.Id{Id: mgrsFiltered.GetManagers()[0].GetId()})
	require.NoError(t, err)
	originalCredID := mgrRead.GetCredentialId()

	mgrRead.CredentialData = []*common.Kv{
		{Key: "AWS_ACCESS_KEY_ID", Value: os.Getenv("AWS_ACCESS_KEY_ID")},
		{Key: "AWS_SECRET_ACCESS_KEY", Value: os.Getenv("AWS_SECRET_ACCESS_KEY")},
		{Key: "AWS_SESSION_TOKEN", Value: os.Getenv("AWS_SESSION_TOKEN")},
	}

	_, err = mgrClient.Update(ctx, mgrRead)
	mgrRead, err = mgrClient.Read(ctx, &manager.Id{Id: mgrRead.GetId()})
	require.NoError(t, err)
	assert.Equal(t, originalCredID, mgrRead.GetCredentialId())

	t.Log("rerunning a nodemanager works")
	_, err = mgrClient.ConnectManager(ctx, &manager.Id{Id: mgrRead.GetId()})
	require.NoError(t, err)
	mgrRead, err = mgrClient.Read(ctx, &manager.Id{Id: mgrRead.GetId()})
	assert.Equal(t, "reachable", mgrRead.Status)

	t.Log("rerunning the Automate nodemanager does nothing")
	_, err = mgrClient.ConnectManager(ctx, &manager.Id{Id: mgrtypes.AutomateManagerID})
	require.NoError(t, err)
	mgrRead, err = mgrClient.Read(ctx, &manager.Id{Id: mgrRead.GetId()})
	assert.Equal(t, "reachable", mgrRead.Status)
}

func TestNodemanagersDelete(t *testing.T) {
	ctx := context.Background()

	mgrConn, err := mgrtesthelpers.GetManagerConn()
	require.NoError(t, err)
	defer mgrConn.Close()

	// setup clients
	mgrClient := manager.NewNodeManagerServiceClient(mgrConn)
	nodesClient := nodes.NewNodesServiceClient(mgrConn)

	t.Log("list nodemanagers, sorted by name")
	mgrs, err := mgrClient.List(ctx, &manager.Query{
		Sort: "name",
	})
	require.NoError(t, err)
	if mgrs.GetTotal() != int32(4) {
		t.Logf("expected four nodemanagers. found %d:something is wrong.", mgrs.GetTotal())
		t.FailNow()
	}
	mgrID1 := mgrs.GetManagers()[1].GetId()
	mgrID2 := mgrs.GetManagers()[2].GetId()
	mgrID3 := mgrs.GetManagers()[3].GetId()

	t.Log("delete nodemanager does not delete nodes")
	list, err := nodesClient.List(ctx, &nodes.Query{})
	require.NoError(t, err)
	totalBeforeDelete := list.GetTotal()

	_, err = mgrClient.Delete(ctx, &manager.Id{Id: mgrID1})
	require.NoError(t, err)

	list, err = nodesClient.List(ctx, &nodes.Query{})
	require.NoError(t, err)

	assert.Equal(t, totalBeforeDelete, list.GetTotal())

	t.Log("delete nodemanager and delete nodes")
	query := nodes.Query{
		Filters: []*common.Filter{
			{Key: "manager_id", Values: []string{mgrID2}},
		},
	}
	list, err = nodesClient.List(ctx, &query)
	require.NoError(t, err)
	totalBeforeDeleteWithNodes := list.GetTotal()

	ids, err := mgrClient.DeleteWithNodes(ctx, &manager.Id{Id: mgrID2})
	require.NoError(t, err)
	assert.NotEqual(t, 0, ids.GetIds())

	list, err = nodesClient.List(ctx, &nodes.Query{})
	require.NoError(t, err)
	assert.Equal(t, list.GetTotal(), totalBeforeDelete-totalBeforeDeleteWithNodes)

	t.Log("delete nodemanager and update manager nodes' state")
	_, err = mgrClient.DeleteWithNodeStateTerminated(ctx, &manager.Id{Id: mgrID3})
	require.NoError(t, err)

	query = nodes.Query{
		Filters: []*common.Filter{
			{Key: "manager_id", Values: []string{mgrID3}},
		},
	}
	list, err = nodesClient.List(ctx, &query)
	require.NoError(t, err)
	for _, node := range list.GetNodes() {
		assert.Equal(t, "terminated", node.GetState())
	}
}
