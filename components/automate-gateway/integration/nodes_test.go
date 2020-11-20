package integration

// import (
// 	"fmt"
// 	"time"

// 	"github.com/chef/automate/api/external/common/query"
// 	gwnodes "github.com/chef/automate/api/external/nodes"
// 	"github.com/chef/automate/api/external/secrets"
// 	"github.com/chef/automate/api/interservice/compliance/jobs"
// 	"github.com/chef/automate/api/interservice/nodemanager/nodes"
// 	"github.com/chef/automate/lib/pcmp/passert"
// )

// func (suite *GatewayTestSuite) TestGatewayNodesClient() {
// 	// get the gateway nodes client (this is where we trigger detect job)
// 	gatewayNodesClient := gwnodes.NewNodesServiceClient(suite.gwConn)

// 	// setup secrets service for secret creation
// 	secretsClient, err := suite.clients.SecretClient()
// 	suite.Require().NoError(err)

// 	// setup compliance-service clients for job creation
// 	jobsClient, err := suite.clients.ComplianceJobsServiceClient()
// 	suite.Require().NoError(err)

// 	// setup nodemanager-service clients for node creation
// 	nodesClient, err := suite.clients.NodesClient()
// 	suite.Require().NoError(err)

// 	// delete all jobs so we start with a clean slate
// 	jobsList, err := jobsClient.List(suite.ctx, &jobs.Query{})
// 	suite.Require().NoError(err)
// 	for _, job := range jobsList.Jobs {
// 		_, err := jobsClient.Delete(suite.ctx, &jobs.Id{Id: job.Id})
// 		suite.Require().NoError(err)
// 	}

// 	fmt.Println("creating secrets for the nodes tests")
// 	// create secret for node
// 	secretID, err := secretsClient.Create(suite.ctx, &secrets.Secret{
// 		Name: "test secret",
// 		Type: "ssh",
// 		Data: []*query.Kv{
// 			{Key: "username", Value: suite.target.User},
// 			{Key: "key", Value: suite.target.Key},
// 		},
// 	})
// 	suite.Require().NoError(err)

// 	// create bad secret for testing
// 	badSecretID, err := secretsClient.Create(suite.ctx, &secrets.Secret{
// 		Name: "test secret",
// 		Type: "ssh",
// 		Data: []*query.Kv{
// 			{Key: "username", Value: suite.target.User},
// 			{Key: "password", Value: "definitely not the password"},
// 		},
// 	})
// 	suite.Require().NoError(err)

// 	fmt.Println("creating node with no auto detect tag")
// 	// create node
// 	nodeID, err := gatewayNodesClient.Create(suite.ctx, &gwnodes.Node{
// 		Name: "test node",
// 		Tags: []*query.Kv{
// 			{Key: "_no_auto_detect", Value: "true"},
// 		},
// 		TargetConfig: &gwnodes.TargetConfig{
// 			Backend: "ssh",
// 			Host:    suite.target.Host,
// 			Port:    22,
// 			Secrets: []string{badSecretID.GetId()},
// 		},
// 	})
// 	suite.Require().NoError(err)

// 	fmt.Println("checking node status: expected unknown")
// 	// no detect job should have been triggered, reading node should have status unknown
// 	node, err := nodesClient.Read(suite.ctx, &nodes.Id{Id: nodeID.Id})
// 	suite.Require().NoError(err)
// 	suite.Equal("unknown", node.Status)

// 	fmt.Println("update node to run detect job with bad secret")
// 	// call update on the node id, ensure detect job was triggered
// 	_, err = gatewayNodesClient.Update(suite.ctx, &gwnodes.Node{
// 		Id:   nodeID.Id,
// 		Name: "test node",
// 		Tags: []*query.Kv{
// 			{Key: "_no_auto_detect", Value: "false"},
// 		},
// 		TargetConfig: &gwnodes.TargetConfig{
// 			Backend: "ssh",
// 			Host:    suite.target.Host,
// 			Port:    22,
// 			Secrets: []string{badSecretID.GetId()},
// 		},
// 	})
// 	suite.Require().NoError(err)

// 	fmt.Println("checking node status, expected unreachable")
// 	nodeStatus := "unknown"
// 	counter := 0
// 	for nodeStatus == "unknown" {
// 		// read gateway compliance nodes to ensure node was created, has a status of unreachable b/c we used bad secret
// 		node, err = nodesClient.Read(suite.ctx, &nodes.Id{Id: nodeID.GetId()})
// 		suite.Require().NoError(err)
// 		nodeStatus = node.Status
// 		counter++
// 		time.Sleep(1 * time.Second)
// 		if counter == 60 {
// 			suite.T().Log("timed out waiting for node at line 112")
// 			suite.T().Fail()
// 		}
// 	}
// 	suite.Equal("unreachable", node.Status)

// 	fmt.Println("update node to run detect job with good secret")
// 	// call update on the node id, ensure detect job was triggered
// 	_, err = gatewayNodesClient.Update(suite.ctx, &gwnodes.Node{
// 		Id:   nodeID.Id,
// 		Name: "test node",
// 		TargetConfig: &gwnodes.TargetConfig{
// 			Backend: "ssh",
// 			Host:    suite.target.Host,
// 			Port:    22,
// 			Secrets: []string{secretID.GetId()},
// 		},
// 	})
// 	suite.Require().NoError(err)

// 	fmt.Println("checking node status, expected reachable, with completed job data")

// 	nodeStatus = "unreachable"
// 	counter = 0
// 	for nodeStatus == "unreachable" {
// 		node, err = nodesClient.Read(suite.ctx, &nodes.Id{Id: nodeID.Id})
// 		suite.Require().NoError(err)
// 		nodeStatus = node.Status
// 		counter++
// 		time.Sleep(1 * time.Second)
// 		if counter == 120 {
// 			suite.T().Log("timed out waiting for node at line 143")
// 			suite.T().Fail()
// 		}
// 	}

// 	node.LastJob.JobId = ""
// 	node.LastJob.EndTime = nil
// 	node.LastJob.StartTime = nil
// 	suite.Equal(nodes.ResultsRow{
// 		NodeId: nodeID.Id,
// 		Status: "completed",
// 	}, *node.LastJob)

// 	tc := *node.TargetConfig
// 	suite.Equal(1, len(tc.KeyFiles))
// 	tc.KeyFiles = []string{}
// 	suite.Equal(nodes.TargetConfig{
// 		Backend:  "ssh",
// 		Host:     suite.target.Host,
// 		Port:     22,
// 		Secrets:  []string{secretID.GetId()},
// 		User:     suite.target.User,
// 		KeyFiles: []string{},
// 	}, tc)

// 	node.LastContact = nil
// 	node.LastJob = nil
// 	node.TargetConfig = nil
// 	passert.Equal(suite.T(), &nodes.Node{
// 		ConnectionError: "authentication failed\n\nAuthentication failed for inspec-target-rhel7-dev.cd.chef.co\n\nNet::SSH::AuthenticationFailed",
// 		Id:              nodeID.Id,
// 		Name:            "test node",
// 		Manager:         "automate",
// 		Platform:        "redhat",
// 		PlatformVersion: "7.7",
// 		ManagerIds:      []string{"e69dc612-7e67-43f2-9b19-256afd385820"},
// 		Status:          "reachable",
// 		State:           "RUNNING",
// 		ScanData:        &nodes.LastContactData{},
// 		RunData:         &nodes.LastContactData{},
// 		CloudInfo:       &nodes.CloudInfo{},
// 	}, node)

// 	jobsList, err = jobsClient.List(suite.ctx, &jobs.Query{})
// 	suite.Require().NoError(err)
// 	suite.Equal(int32(2), jobsList.Total)

// 	fmt.Println("call rerun on the node to ensure detect job is created")
// 	// call rerun on the node id, ensure another detect job was triggered
// 	_, err = gatewayNodesClient.Rerun(suite.ctx, &gwnodes.Id{Id: nodeID.Id})
// 	suite.Require().NoError(err)

// 	jobsList, err = jobsClient.List(suite.ctx, &jobs.Query{})
// 	suite.Require().NoError(err)
// 	suite.Equal(int32(3), jobsList.Total)

// 	fmt.Println("create node with bulk create endpoint, no auto detect")
// 	// test bulk create - no detect job
// 	ids, err := gatewayNodesClient.BulkCreate(suite.ctx, &gwnodes.Nodes{
// 		Nodes: []*gwnodes.Node{{
// 			Name: "test node",
// 			Tags: []*query.Kv{
// 				{Key: "_no_auto_detect", Value: "true"},
// 			},
// 			TargetConfig: &gwnodes.TargetConfig{
// 				Backend: "ssh",
// 				Hosts:   []string{suite.target.Host},
// 				Port:    22,
// 				Secrets: []string{secretID.GetId()},
// 			},
// 		}},
// 	})
// 	suite.Require().NoError(err)
// 	suite.Equal(1, len(ids.GetIds()))

// 	fmt.Println("checking node status, expected unknown")
// 	node, err = nodesClient.Read(suite.ctx, &nodes.Id{Id: ids.GetIds()[0]})
// 	suite.Require().NoError(err)
// 	suite.Equal("unknown", node.Status)

// 	fmt.Println("create node with bulk create endpoint, with auto detect")
// 	// test bulk create - with detect job
// 	ids, err = gatewayNodesClient.BulkCreate(suite.ctx, &gwnodes.Nodes{
// 		Nodes: []*gwnodes.Node{{
// 			Name: "test node",
// 			Tags: []*query.Kv{},
// 			TargetConfig: &gwnodes.TargetConfig{
// 				Backend: "ssh",
// 				Hosts:   []string{suite.target.Host},
// 				Port:    22,
// 				Secrets: []string{secretID.GetId()},
// 			},
// 		}},
// 	})
// 	suite.Require().NoError(err)
// 	suite.Equal(1, len(ids.GetIds()))

// 	fmt.Println("checking node status, expected reachable")
// 	nodeStatus = "unknown"
// 	counter = 0
// 	for nodeStatus == "unknown" {
// 		// read gateway compliance nodes to ensure node was created, has a status of unreachable b/c we used bad secret
// 		node, err = nodesClient.Read(suite.ctx, &nodes.Id{Id: ids.GetIds()[0]})
// 		suite.Require().NoError(err)
// 		nodeStatus = node.Status
// 		counter++
// 		time.Sleep(1 * time.Second)
// 		if counter == 120 {
// 			suite.T().Log("timed out waiting for node at line 250")
// 			suite.T().Fail()
// 		}
// 	}
// 	suite.Equal("reachable", node.Status)
// }
