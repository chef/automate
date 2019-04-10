//
//  Author:: Salim Afiune <afiune@chef.io>
//  Copyright:: Copyright 2017, Chef Software Inc.
//

package integration_test

import (
	"context"
	"testing"

	"github.com/stretchr/testify/assert"
	"google.golang.org/grpc/codes"

	"github.com/chef/automate/api/interservice/cfgmgmt/request"
	"github.com/chef/automate/api/interservice/cfgmgmt/response"
	iBackend "github.com/chef/automate/components/ingest-service/backend"
	"github.com/chef/automate/lib/grpc/grpctest"
)

// A very basic test that doesn't require data ingestion
func TestStatsNodesCountsEmpty(t *testing.T) {
	// To make any rpc request you need at least:
	//
	// * Generate the request to test
	// * Genarate a context
	ctx := context.Background()
	req := request.NodesCounts{}

	// From here you can just call any function and that would be
	// just like if you were Dialing-in, it will give you and error
	// and a response that you can then assert against
	//
	// NOTE: 'cfgmgmt' is coming from 'global_test.go'
	res, err := cfgmgmt.GetNodesCounts(ctx, &req)

	// This is our expected response
	expected := &response.NodesCounts{}

	// Assert the results
	assert.Nil(t, err)
	assert.Equal(t, expected, res)
}

// A more complex test that requires data ingestion
func TestStatsNodesCountsWithTwoNodes(t *testing.T) {
	// An example on how to initialize data into the backend so
	// you have something to assert against
	//
	// Generate the objects you want to ingest
	nodes := []iBackend.Node{
		iBackend.Node{
			NodeInfo: iBackend.NodeInfo{
				EntityUuid:       newUUID(),
				Status:           "success",
				NodeName:         "node1",
				OrganizationName: "org1",
				Environment:      "env1",
			},
			Exists: true,
		},
		iBackend.Node{
			NodeInfo: iBackend.NodeInfo{
				EntityUuid:       newUUID(),
				Status:           "missing",
				NodeName:         "node2",
				OrganizationName: "org1",
				Environment:      "env1",
			},
			Exists: true,
		},
	}

	// Ingest the nodes, this will automatically refresh the indexes
	suite.IngestNodes(nodes)

	// Immediately after the ingestion add the hook to clean all documents,
	// with the defer word you will ensure that the next test will have
	// clean data regardless if this test passes or fails
	defer suite.DeleteAllDocuments()

	// As the first example you need the same two things
	ctx := context.Background()
	req := request.NodesCounts{} // Same request, but we can pass more things like filters

	// Lets call our rpc method that we are testing
	// (remember this the same as dialing-in)
	res, err := cfgmgmt.GetNodesCounts(ctx, &req)

	// This is our expected response, now we have one node missing
	// and one successful node, that is a total of two nodes
	expected := &response.NodesCounts{
		Total:   2,
		Missing: 1,
		Success: 1,
	}

	// Assert the results
	assert.Nil(t, err)
	assert.Equal(t, expected, res)
}

// A complete tests with data ingestion and request filtering/parameters
func TestStatsNodesCountsWhen20Nodes(t *testing.T) {
	var nNodes = 20
	var nodes = make([]iBackend.Node, nNodes)

	// Generate 20[nNodes] node objects to ingest
	for i := 0; i < nNodes; i++ {
		nodes[i] = iBackend.Node{
			NodeInfo: iBackend.NodeInfo{
				EntityUuid: newUUID(),
				Status:     "success",
				Platform:   "ubuntu",
			},
			Exists: true,
		}
	}

	// Ingest the nodes
	suite.IngestNodes(nodes)

	// Remember to clean the documents
	defer suite.DeleteAllDocuments()

	// The same two things
	ctx := context.Background()
	req := request.NodesCounts{} // We will modify this request

	// Lets call our rpc method that we are testing
	res, err := cfgmgmt.GetNodesCounts(ctx, &req)

	// The expected response
	expected := &response.NodesCounts{
		Total:   int32(nNodes),
		Success: int32(nNodes),
	}

	// Assert the results
	assert.Nil(t, err)
	assert.Equal(t, expected, res)

	// Now lets modify the request with a few filters and call our rpc method again
	req.Filter = []string{"platform:ubuntu", "status:success"}
	res, err = cfgmgmt.GetNodesCounts(ctx, &req)

	// This will give the same exact expected response
	assert.Nil(t, err)
	assert.Equal(t, expected, res)

	// But it will return 0 Nodes if we filter by 'platform:windows'
	req.Filter = []string{"platform:windows"}
	res, err = cfgmgmt.GetNodesCounts(ctx, &req)
	expected = &response.NodesCounts{} // This is the same as Total:0 Success:0 ...etc

	// This will give the same exact expected response
	assert.Nil(t, err)
	assert.Equal(t, expected, res)
}

// Now I think you detected the pattern!! :smile:
//
// Lets add even more robust tests by creating a
// table driven sub-test to cover all code paths
func TestStatsNodesCountsFilteringWithTableDriven(t *testing.T) {
	dataNodes := []struct {
		number int
		node   iBackend.NodeInfo
	}{
		{10, iBackend.NodeInfo{Status: "success", Platform: "windows"}},
		{10, iBackend.NodeInfo{Status: "failure", Platform: "ubuntu"}},
		{10, iBackend.NodeInfo{Status: "success", Platform: "centos"}},
		{10, iBackend.NodeInfo{Status: "missing", Platform: "centos"}},
		{1, iBackend.NodeInfo{NodeName: "mock", Status: "success"}},
	}

	var totalNodes int32 = 0
	var nodes = make([]iBackend.Node, totalNodes)

	for _, data := range dataNodes {
		for i := 0; i < data.number; i++ {
			// Generate a uuid
			data.node.EntityUuid = newUUID()
			node := iBackend.Node{
				NodeInfo: data.node,
				Exists:   true,
			}
			nodes = append(nodes, node)
			// Increment the total number of nodes
			totalNodes++
		}
	}

	suite.IngestNodes(nodes)
	defer suite.DeleteAllDocuments()

	// Now we are going to use a table of different tests cases
	// where we will have description of the test case, the request
	// we will send to the rpc function we are testing and the expected
	// response that we will assert to have in return. Nice!
	ctx := context.Background()
	cases := []struct {
		description string
		request     request.NodesCounts
		expected    response.NodesCounts
	}{
		{"should return all nodes",
			request.NodesCounts{}, // Empty
			response.NodesCounts{ // All nodes
				Total:   totalNodes,
				Success: 21,
				Failure: 10,
				Missing: 10,
			}},
		{"should return only 10 ubuntu nodes",
			request.NodesCounts{Filter: []string{"platform:ubuntu"}},
			response.NodesCounts{Total: 10, Failure: 10}},
		{"should return only 21 successful nodes",
			request.NodesCounts{Filter: []string{"status:success"}},
			response.NodesCounts{Total: 21, Success: 21}},
		{"should return only 10 missing centos nodes",
			request.NodesCounts{
				Filter: []string{"status:missing", "platform:centos"},
			},
			response.NodesCounts{Total: 10, Missing: 10}},
		{"should return 0 successful ubuntu nodes",
			request.NodesCounts{
				Filter: []string{"status:success", "platform:ubuntu"},
			},
			response.NodesCounts{}},
		{"should return 0 oracle nodes",
			request.NodesCounts{Filter: []string{"platform:oracle"}},
			response.NodesCounts{}},
		{"should return only the 'mock' node",
			request.NodesCounts{Filter: []string{"name:mock"}},
			response.NodesCounts{Total: 1, Success: 1}},
	}

	// Run all the cases!
	for _, test := range cases {
		t.Logf("\nwith filter(s) %v\n -> it %s", test.request.GetFilter(), test.description)
		res, err := cfgmgmt.GetNodesCounts(ctx, &test.request)
		assert.Nil(t, err)
		assert.Equal(t, test.expected, *res)
	}
}

// When testing request errors like wrong parameters, we do not need
// any ES data, this makes me thing that this might actually be a unit
// test more than an integration test. (prob move to 'grpcserver/cfg_mgmt.go'?)
func TestStatsNodesCountsWrongParameters(t *testing.T) {
	ctx := context.Background()

	cases := []struct {
		request request.NodesCounts
	}{
		{request.NodesCounts{Filter: []string{"platform=centos"}}},
		{request.NodesCounts{Filter: []string{"wrong"}}},
		{request.NodesCounts{Filter: []string{":success"}}},
		{request.NodesCounts{Filter: []string{"platform:"}}},
		{request.NodesCounts{Filter: []string{"platform:foo:bar"}}},
	}

	// Run all the cases!
	for _, test := range cases {
		t.Logf("\nwith filter(s) %v\n -> it should return an error", test.request.GetFilter())
		res, err := cfgmgmt.GetNodesCounts(ctx, &test.request)
		assert.NotNil(t, err)
		grpctest.AssertCode(t, codes.InvalidArgument, err)
		assert.Nil(t, res)
	}
}
