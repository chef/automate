package manager

import (
	"context"
	"testing"

	"github.com/chef/automate/api/external/common/query"
	"github.com/chef/automate/api/external/secrets"
	"github.com/chef/automate/api/interservice/compliance/common"
	"github.com/chef/automate/api/interservice/nodemanager/nodes"
	"github.com/chef/automate/components/nodemanager-service/mgrtypes"
	"github.com/chef/automate/components/nodemanager-service/tests/mgrtesthelpers"
	"github.com/golang/protobuf/ptypes/timestamp"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func setup() (context.Context, string, error) {
	ctx := context.Background()
	secretsConn, err := mgrtesthelpers.GetSecretsConn()
	if err != nil {
		return ctx, "", err
	}
	defer secretsConn.Close()

	secretsClient := secrets.NewSecretsServiceClient(secretsConn)
	id, err := secretsClient.Create(ctx, &secrets.Secret{
		Name: "test-ssh",
		Type: "ssh",
		Data: []*query.Kv{
			{Key: "username", Value: "test-user"},
			{Key: "password", Value: "test-password"},
		},
	})
	return ctx, id.GetId(), err
}

func TestNodesErrorCases(t *testing.T) {
	t.Log("create a credential for reference from the node")
	ctx, secretID, err := setup()
	require.NoError(t, err)

	// get nodes client
	t.Log("setup nodes client")
	nodesConn, err := mgrtesthelpers.GetManagerConn()
	require.NoError(t, err)
	defer nodesConn.Close()
	nodesClient := nodes.NewNodesServiceClient(nodesConn)

	// delete all nodes
	t.Log("delete all nodes")
	err = mgrtesthelpers.DeleteAllNodes(ctx, nodesClient)
	require.NoError(t, err)

	// ERROR CASES
	t.Log("test error cases")

	// read missing node, expect err
	_, err = nodesClient.Read(ctx, &nodes.Id{Id: "missing"})
	assert.Contains(t, err.Error(), "Not found for id: missing")

	// list with invalid sort field, expect err
	_, err = nodesClient.List(ctx, &nodes.Query{Sort: "wrong"})
	assert.Contains(t, err.Error(), "Invalid sort field, valid ones are: [last_contact manager name platform platform_version state status]")

	// create node with invalid port for ssh, expect err
	_, err = nodesClient.Create(ctx, &nodes.Node{
		Name:    "M$",
		Manager: "automate",
		TargetConfig: &nodes.TargetConfig{
			Backend: "ssh",
			Host:    "5.6.7.8",
			Secrets: []string{secretID},
		},
		Tags: []*common.Kv{},
	})
	assert.Contains(t, err.Error(), "Invalid node. Port for node must be within range: 1-65535")

	// create node with invalid port for winrm, expect err
	_, err = nodesClient.Create(ctx, &nodes.Node{
		Name:    "A node",
		Manager: "automate",
		TargetConfig: &nodes.TargetConfig{
			Backend: "winrm",
			Host:    "5.6.7.8",
			Secrets: []string{secretID},
			Sudo:    true,
			Port:    0,
		},
		Tags: []*common.Kv{},
	})
	assert.Contains(t, err.Error(), "Invalid node. Port for node must be within range: 1-65535")

	// create a node with no name, expect err
	_, err = nodesClient.Create(ctx, &nodes.Node{
		Name:    "",
		Manager: "automate",
		TargetConfig: &nodes.TargetConfig{
			Backend: "ssh",
			Host:    "5.6.7.8",
			Secrets: []string{secretID},
			Port:    22,
		},
		Tags: []*common.Kv{},
	})
	assert.Contains(t, err.Error(), "Invalid node, 'name' is a required parameter")

	// create node with missing and real secret
	_, err = nodesClient.Create(ctx, &nodes.Node{
		Name:    "fake-name",
		Manager: "automate",
		TargetConfig: &nodes.TargetConfig{
			Backend: "ssh",
			Host:    "5.6.7.8",
			Secrets: []string{"1234545632", secretID},
			Port:    22,
		},
		Tags: []*common.Kv{},
	})
	assert.Contains(t, err.Error(), "AddNode unable to confirm secrets existence")
}

func TestNodesCreateAndReadAndListCases(t *testing.T) {
	t.Log("create a credential for reference from the node")
	ctx, secretID, err := setup()
	require.NoError(t, err)

	// get nodes client
	t.Log("setup nodes client")
	nodesConn, err := mgrtesthelpers.GetManagerConn()
	require.NoError(t, err)
	defer nodesConn.Close()
	nodesClient := nodes.NewNodesServiceClient(nodesConn)

	// delete all nodes
	t.Log("delete all nodes")
	err = mgrtesthelpers.DeleteAllNodes(ctx, nodesClient)
	require.NoError(t, err)

	// list nodes, expect 0
	nodesRes, err := nodesClient.List(ctx, &nodes.Query{})
	require.NoError(t, err)
	assert.Equal(t, int32(0), nodesRes.Total)

	// test create cases
	t.Log("test create cases")

	// create node with tags
	node2, err := nodesClient.Create(ctx, &nodes.Node{
		Name:    "cows",
		Manager: "automate",
		TargetConfig: &nodes.TargetConfig{
			Backend: "winrm",
			Host:    "5.6.7.8",
			Secrets: []string{secretID},
			Port:    22,
		},
		Tags: []*common.Kv{
			{Key: "environment", Value: "bear"},
			{Key: "project", Value: "bear mouse"},
		},
	})
	require.NoError(t, err)

	// create another node
	node3, err := nodesClient.Create(ctx, &nodes.Node{
		Name:    "zeta zoo",
		Manager: "automate",
		TargetConfig: &nodes.TargetConfig{
			Backend: "ssh",
			Host:    "5.6.7.8",
			Secrets: []string{secretID},
			Port:    22,
			Sudo:    true,
		},
		Tags: []*common.Kv{
			{Key: "environment", Value: "mouse"},
			{Key: "project", Value: "bear mouse"},
		},
	})
	require.NoError(t, err)

	// test read node
	t.Log("test read node")
	nodeRes, err := nodesClient.Read(ctx, &nodes.Id{Id: node3.GetId()})
	require.NoError(t, err)
	assert.Equal(t, "zeta zoo", nodeRes.GetName())
	assert.Equal(t, &nodes.TargetConfig{
		Backend:    "ssh",
		Host:       "5.6.7.8",
		Port:       22,
		Sudo:       true,
		Ssl:        false,
		SelfSigned: false,
		User:       "test-user",
		Password:   "test-password",
		Secrets:    []string{secretID},
	}, nodeRes.GetTargetConfig())

	// test list cases
	t.Log("test list cases")
	nodesRes, err = nodesClient.List(ctx, &nodes.Query{})
	require.NoError(t, err)
	expectedNodes := []*nodes.Node{
		{
			Id:      node2.GetId(),
			Name:    "cows",
			Manager: "automate",
			Tags: []*common.Kv{
				{Key: "environment", Value: "bear"},
				{Key: "project", Value: "bear mouse"},
			},
			Status:     "unknown",
			ManagerIds: []string{mgrtypes.AutomateManagerID},
			LastContact: &timestamp.Timestamp{
				Seconds: -62135596800,
			},
			RunData:  &nodes.LastContactData{},
			ScanData: &nodes.LastContactData{},
		},
		{
			Id:      node3.GetId(),
			Name:    "zeta zoo",
			Manager: "automate",
			Tags: []*common.Kv{
				{Key: "environment", Value: "mouse"},
				{Key: "project", Value: "bear mouse"},
			},
			Status:     "unknown",
			ManagerIds: []string{mgrtypes.AutomateManagerID},
			LastContact: &timestamp.Timestamp{
				Seconds: -62135596800,
			},
			RunData:  &nodes.LastContactData{},
			ScanData: &nodes.LastContactData{},
		},
	}
	assert.Equal(t, expectedNodes, nodesRes.GetNodes())

	t.Log("test sort and filter")
	// sort by name
	nodesRes, err = nodesClient.List(ctx, &nodes.Query{Sort: "name", Order: nodes.Query_DESC})
	require.NoError(t, err)
	assert.Equal(t, "cows", nodesRes.GetNodes()[1].Name)
	assert.Equal(t, "zeta zoo", nodesRes.GetNodes()[0].Name)

	// sort by name, pagination, ensure total count is correct
	nodesRes, err = nodesClient.List(ctx, &nodes.Query{Sort: "name", Order: nodes.Query_DESC, Page: 1, PerPage: 1})
	require.NoError(t, err)
	assert.Equal(t, 1, len(nodesRes.GetNodes()))
	assert.Equal(t, "zeta zoo", nodesRes.GetNodes()[0].GetName())
	assert.Equal(t, int32(2), nodesRes.GetTotal())
}

func TestNodesUpdateCases(t *testing.T) {
	t.Log("create a credential for reference from the node")
	ctx, secretID, err := setup()
	require.NoError(t, err)

	// get nodes client
	t.Log("setup nodes client")
	nodesConn, err := mgrtesthelpers.GetManagerConn()
	require.NoError(t, err)
	defer nodesConn.Close()
	nodesClient := nodes.NewNodesServiceClient(nodesConn)

	t.Log("create a node")
	node, err := nodesClient.Create(ctx, &nodes.Node{
		Name:    "bears",
		Manager: "automate",
		TargetConfig: &nodes.TargetConfig{
			Backend: "ssh",
			Host:    "5.3.9.8",
			Secrets: []string{secretID},
			Port:    22,
			Sudo:    true,
		},
		Tags: []*common.Kv{
			{Key: "environment", Value: "mouse"},
			{Key: "project", Value: "bear mouse"},
		},
	})
	require.NoError(t, err)

	t.Log("test update")

	// update node name
	_, err = nodesClient.Update(ctx, &nodes.Node{
		Id:      node.GetId(),
		Name:    "clown",
		Manager: "automate",
		TargetConfig: &nodes.TargetConfig{
			Backend: "ssh",
			Host:    "5.3.9.8",
			Secrets: []string{secretID},
			Port:    22,
			Sudo:    true,
		},
		Tags: []*common.Kv{
			{Key: "environment", Value: "mouse"},
			{Key: "project", Value: "bear mouse"},
		},
	})
	require.NoError(t, err)
	nodeRead, err := nodesClient.Read(ctx, &nodes.Id{Id: node.GetId()})
	require.NoError(t, err)
	assert.Equal(t, "clown", nodeRead.GetName())

	// update node secret, expect err
	_, err = nodesClient.Update(ctx, &nodes.Node{
		Id:      node.GetId(),
		Name:    "bears",
		Manager: "automate",
		TargetConfig: &nodes.TargetConfig{
			Backend: "ssh",
			Host:    "5.3.9.8",
			Secrets: []string{"12323413"},
			Port:    22,
			Sudo:    true,
		},
		Tags: []*common.Kv{
			{Key: "environment", Value: "mouse"},
			{Key: "project", Value: "bear mouse"},
		},
	})
	assert.Contains(t, err.Error(), "AddNode unable to confirm secrets existence")

	// update node target config
	_, err = nodesClient.Update(ctx, &nodes.Node{
		Id:      node.GetId(),
		Name:    "clown",
		Manager: "automate",
		TargetConfig: &nodes.TargetConfig{
			Backend: "ssh",
			Host:    "27.12.0.7",
			Secrets: []string{secretID},
			Port:    22,
			Sudo:    false,
		},
		Tags: []*common.Kv{
			{Key: "environment", Value: "mouse"},
			{Key: "project", Value: "bear mouse"},
		},
	})
	require.NoError(t, err)
	nodeRead, err = nodesClient.Read(ctx, &nodes.Id{Id: node.GetId()})
	require.NoError(t, err)
	assert.Equal(t, "27.12.0.7", nodeRead.GetTargetConfig().GetHost())
	assert.Equal(t, false, nodeRead.GetTargetConfig().GetSudo())

	// update node tags
	_, err = nodesClient.Update(ctx, &nodes.Node{
		Id:      node.GetId(),
		Name:    "clown",
		Manager: "automate",
		TargetConfig: &nodes.TargetConfig{
			Backend: "ssh",
			Host:    "27.12.0.7",
			Secrets: []string{secretID},
			Port:    22,
			Sudo:    false,
		},
		Tags: []*common.Kv{
			{Key: "environment", Value: "boo"},
		},
	})
	require.NoError(t, err)
	nodeRead, err = nodesClient.Read(ctx, &nodes.Id{Id: node.GetId()})
	require.NoError(t, err)
	assert.Equal(t, []*common.Kv{
		{Key: "environment", Value: "boo"},
	}, nodeRead.GetTags())
}

func TestNodesBulkCreate(t *testing.T) {
	t.Log("create a credential for reference from the node")
	ctx, secretID, err := setup()
	require.NoError(t, err)

	// get nodes client
	t.Log("setup nodes client")
	nodesConn, err := mgrtesthelpers.GetManagerConn()
	require.NoError(t, err)
	defer nodesConn.Close()
	nodesClient := nodes.NewNodesServiceClient(nodesConn)

	// delete all nodes
	t.Log("delete all nodes")
	err = mgrtesthelpers.DeleteAllNodes(ctx, nodesClient)
	require.NoError(t, err)

	t.Log("test bulk create")
	// Add nodes via bulk create; this should create three nodes
	ids, err := nodesClient.BulkCreate(ctx, &nodes.Nodes{
		Nodes: []*nodes.Node{
			{
				NamePrefix: "my-ssh-node",
				Manager:    "automate",
				TargetConfig: &nodes.TargetConfig{
					Backend: "ssh",
					Hosts:   []string{"localhost", "127.0.0.1"},
					Secrets: []string{secretID},
					Port:    22,
				},
				Tags: []*common.Kv{
					{Key: "test-node", Value: "is-amazing"},
					{Key: "compliance-service", Value: "rockin-like-whoa"},
				},
			},
			{
				NamePrefix: "my-other-node",
				Manager:    "automate",
				TargetConfig: &nodes.TargetConfig{
					Backend: "winrm",
					Hosts:   []string{"localhost"},
					Secrets: []string{secretID},
					Port:    5986,
				},
				Tags: []*common.Kv{
					{Key: "test-node", Value: "is-more-amazing"},
				},
			},
		},
	})
	require.NoError(t, err)

	nodesRes, err := nodesClient.List(ctx, &nodes.Query{Sort: "name", Order: nodes.Query_ASC})
	require.NoError(t, err)
	assert.Equal(t, int32(3), nodesRes.GetTotal())

	assert.Equal(t, []*nodes.Node{
		{
			Id:      ids.GetIds()[2],
			Name:    "my-other-node-localhost",
			Manager: "automate",
			Tags: []*common.Kv{
				{Key: "test-node", Value: "is-more-amazing"},
			},
			Status:     "unknown",
			ManagerIds: []string{mgrtypes.AutomateManagerID},
			LastContact: &timestamp.Timestamp{
				Seconds: -62135596800,
			},
			RunData:  &nodes.LastContactData{},
			ScanData: &nodes.LastContactData{},
		},
		{
			Id:      ids.GetIds()[1],
			Name:    "my-ssh-node-127.0.0.1",
			Manager: "automate",
			Tags: []*common.Kv{
				{Key: "compliance-service", Value: "rockin-like-whoa"},
				{Key: "test-node", Value: "is-amazing"},
			},
			Status:     "unknown",
			ManagerIds: []string{mgrtypes.AutomateManagerID},
			LastContact: &timestamp.Timestamp{
				Seconds: -62135596800,
			},
			RunData:  &nodes.LastContactData{},
			ScanData: &nodes.LastContactData{},
		},
		{
			Id:      ids.GetIds()[0],
			Name:    "my-ssh-node-localhost",
			Manager: "automate",
			Tags: []*common.Kv{
				{Key: "compliance-service", Value: "rockin-like-whoa"},
				{Key: "test-node", Value: "is-amazing"},
			},
			Status:     "unknown",
			ManagerIds: []string{mgrtypes.AutomateManagerID},
			LastContact: &timestamp.Timestamp{
				Seconds: -62135596800,
			},
			RunData:  &nodes.LastContactData{},
			ScanData: &nodes.LastContactData{},
		},
	}, nodesRes.GetNodes())
}
