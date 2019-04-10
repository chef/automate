package compliance

import (
	"context"
	"fmt"
	"os"
	"testing"
	"time"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	"github.com/chef/automate/api/external/secrets"
	gwnodes "github.com/chef/automate/components/automate-gateway/api/nodes"
	"github.com/chef/automate/components/compliance-service/api/common"
	"github.com/chef/automate/components/compliance-service/api/jobs"
	"github.com/chef/automate/components/compliance-service/examples/helpers"
	"github.com/chef/automate/components/nodemanager-service/api/nodes"
)

var host = os.Getenv("AUTOMATE_ACCEPTANCE_TARGET_HOST")

func TestGatewayNodesClient(t *testing.T) {
	complianceEndpoint := "127.0.0.1:10121"
	secretsEndpoint := "127.0.0.1:10131"
	gatewayEndpoint := "127.0.0.1:2000"
	nodesEndpoint := "127.0.0.1:10120"
	ctx := context.Background()
	connFactory := helpers.SecureConnFactoryHabWithDeploymentServiceCerts()

	// get the gateway nodes client (this is where we trigger detect job)
	gatewayConn, err := connFactory.Dial("automate-gateway", gatewayEndpoint)
	require.NoError(t, err)
	defer gatewayConn.Close()
	gatewayNodesClient := gwnodes.NewNodesServiceClient(gatewayConn)

	// setup secrets service for secret creation
	secretsConn, err := connFactory.Dial("secrets-service", secretsEndpoint)
	require.NoError(t, err)
	defer secretsConn.Close()
	secretsClient := secrets.NewSecretsServiceClient(secretsConn)

	// setup compliance-service clients for job creation
	complianceConn, err := connFactory.Dial("compliance-service", complianceEndpoint)
	require.NoError(t, err)
	defer complianceConn.Close()
	jobsClient := jobs.NewJobsServiceClient(complianceConn)

	// setup nodemanager-service clients for node creation
	nodesConn, err := connFactory.Dial("nodemanager-service", nodesEndpoint)
	require.NoError(t, err)
	defer nodesConn.Close()
	nodesClient := nodes.NewNodesServiceClient(nodesConn)

	// delete all jobs so we start with a clean slate
	jobsList, err := jobsClient.List(ctx, &jobs.Query{})
	require.NoError(t, err)
	for _, job := range jobsList.Jobs {
		_, err := jobsClient.Delete(ctx, &jobs.Id{Id: job.Id})
		require.NoError(t, err)
	}

	fmt.Println("creating secrets for the nodes tests")
	// create secret for node
	secretId, err := secretsClient.Create(ctx, &secrets.Secret{
		Name: "test secret",
		Type: "ssh",
		Data: []*secrets.Kv{
			{Key: "username", Value: os.Getenv("AUTOMATE_ACCEPTANCE_TARGET_USERNAME")},
			{Key: "password", Value: os.Getenv("AUTOMATE_ACCEPTANCE_TARGET_PASSWORD")},
		},
	})
	require.NoError(t, err)

	// create bad secret for testing
	badSecretId, err := secretsClient.Create(ctx, &secrets.Secret{
		Name: "test secret",
		Type: "ssh",
		Data: []*secrets.Kv{
			{Key: "username", Value: os.Getenv("AUTOMATE_ACCEPTANCE_TARGET_USERNAME")},
			{Key: "password", Value: "definitely not the password"},
		},
	})
	require.NoError(t, err)

	fmt.Println("creating node with no auto detect tag")
	// create node
	nodeId, err := gatewayNodesClient.Create(ctx, &gwnodes.Node{
		Name: "test node",
		Tags: []*common.Kv{
			{Key: "_no_auto_detect", Value: "true"},
		},
		TargetConfig: &gwnodes.TargetConfig{
			Backend: "ssh",
			Host:    host,
			Port:    22,
			Secrets: []string{badSecretId.GetId()},
		},
	})
	require.NoError(t, err)

	fmt.Println("checking node status: expected unknown")
	// no detect job should have been triggered, reading node should have status unknown
	node, err := nodesClient.Read(ctx, &nodes.Id{Id: nodeId.Id})
	require.NoError(t, err)
	assert.Equal(t, "unknown", node.Status)

	fmt.Println("update node to run detect job with bad secret")
	// call update on the node id, ensure detect job was triggered
	_, err = gatewayNodesClient.Update(ctx, &gwnodes.Node{
		Id:   nodeId.Id,
		Name: "test node",
		Tags: []*common.Kv{
			{Key: "_no_auto_detect", Value: "false"},
		},
		TargetConfig: &gwnodes.TargetConfig{
			Backend: "ssh",
			Host:    host,
			Port:    22,
			Secrets: []string{badSecretId.GetId()},
		},
	})
	require.NoError(t, err)

	fmt.Println("checking node status, expected unreachable")
	nodeStatus := "unknown"
	counter := 0
	for nodeStatus == "unknown" {
		// read gateway compliance nodes to ensure node was created, has a status of unreachable b/c we used bad secret
		node, err = nodesClient.Read(ctx, &nodes.Id{Id: nodeId.GetId()})
		require.NoError(t, err)
		nodeStatus = node.Status
		counter++
		time.Sleep(1 * time.Second)
		if counter == 60 {
			t.Log("timed out waiting for node at line 128")
			t.Fail()
		}
	}
	assert.Equal(t, "unreachable", node.Status)

	fmt.Println("update node to run detect job with good secret")
	// call update on the node id, ensure detect job was triggered
	_, err = gatewayNodesClient.Update(ctx, &gwnodes.Node{
		Id:   nodeId.Id,
		Name: "test node",
		TargetConfig: &gwnodes.TargetConfig{
			Backend: "ssh",
			Host:    host,
			Port:    22,
			Secrets: []string{secretId.GetId()},
		},
	})
	require.NoError(t, err)

	fmt.Println("checking node status, expected reachable, with completed job data")

	nodeStatus = "unreachable"
	counter = 0
	for nodeStatus == "unreachable" {
		node, err = nodesClient.Read(ctx, &nodes.Id{Id: nodeId.Id})
		require.NoError(t, err)
		nodeStatus = node.Status
		counter++
		time.Sleep(1 * time.Second)
		if counter == 120 {
			t.Log("timed out waiting for node at line 159")
			t.Fail()
		}
	}

	node.LastJob.JobId = ""
	node.LastJob.EndTime = nil
	node.LastJob.StartTime = nil
	assert.Equal(t, nodes.ResultsRow{
		NodeId: nodeId.Id,
		Status: "completed",
	}, *node.LastJob)

	assert.Equal(t, nodes.TargetConfig{
		Backend:  "ssh",
		Host:     host,
		Port:     22,
		Secrets:  []string{secretId.GetId()},
		User:     os.Getenv("AUTOMATE_ACCEPTANCE_TARGET_USERNAME"),
		Password: os.Getenv("AUTOMATE_ACCEPTANCE_TARGET_PASSWORD"),
	}, *node.TargetConfig)

	node.LastContact = nil
	node.LastJob = nil
	node.TargetConfig = nil
	assert.Equal(t, &nodes.Node{
		ConnectionError: "authentication failed",
		Id:              nodeId.Id,
		Name:            "test node",
		Manager:         "automate",
		Platform:        "amazon",
		PlatformVersion: "2",
		ManagerIds:      []string{"e69dc612-7e67-43f2-9b19-256afd385820"},
		Status:          "reachable",
		State:           "RUNNING",
	}, node)

	jobsList, err = jobsClient.List(ctx, &jobs.Query{})
	require.NoError(t, err)
	assert.Equal(t, int32(2), jobsList.Total)

	fmt.Println("call rerun on the node to ensure detect job is created")
	// call rerun on the node id, ensure another detect job was triggered
	_, err = gatewayNodesClient.Rerun(ctx, &gwnodes.Id{Id: nodeId.Id})
	require.NoError(t, err)

	jobsList, err = jobsClient.List(ctx, &jobs.Query{})
	require.NoError(t, err)
	assert.Equal(t, int32(3), jobsList.Total)

	fmt.Println("create node with bulk create endpoint, no auto detect")
	// test bulk create - no detect job
	ids, err := gatewayNodesClient.BulkCreate(ctx, &gwnodes.Nodes{
		Nodes: []*gwnodes.Node{{
			Name: "test node",
			Tags: []*common.Kv{
				{Key: "_no_auto_detect", Value: "true"},
			},
			TargetConfig: &gwnodes.TargetConfig{
				Backend: "ssh",
				Hosts:   []string{host},
				Port:    22,
				Secrets: []string{secretId.GetId()},
			},
		}},
	})
	require.NoError(t, err)
	assert.Equal(t, 1, len(ids.GetIds()))

	fmt.Println("checking node status, expected unknown")
	node, err = nodesClient.Read(ctx, &nodes.Id{Id: ids.GetIds()[0]})
	require.NoError(t, err)
	assert.Equal(t, "unknown", node.Status)

	fmt.Println("create node with bulk create endpoint, with auto detect")
	// test bulk create - with detect job
	ids, err = gatewayNodesClient.BulkCreate(ctx, &gwnodes.Nodes{
		Nodes: []*gwnodes.Node{{
			Name: "test node",
			Tags: []*common.Kv{},
			TargetConfig: &gwnodes.TargetConfig{
				Backend: "ssh",
				Hosts:   []string{host},
				Port:    22,
				Secrets: []string{secretId.GetId()},
			},
		}},
	})
	require.NoError(t, err)
	assert.Equal(t, 1, len(ids.GetIds()))

	fmt.Println("checking node status, expected reachable")
	nodeStatus = "unknown"
	counter = 0
	for nodeStatus == "unknown" {
		// read gateway compliance nodes to ensure node was created, has a status of unreachable b/c we used bad secret
		node, err = nodesClient.Read(ctx, &nodes.Id{Id: ids.GetIds()[0]})
		require.NoError(t, err)
		nodeStatus = node.Status
		counter++
		time.Sleep(1 * time.Second)
		if counter == 120 {
			t.Log("timed out waiting for node at line 260")
			t.Fail()
		}
	}
	assert.Equal(t, "reachable", node.Status)
}
