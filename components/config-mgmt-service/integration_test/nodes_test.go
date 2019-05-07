package integration_test

import (
	"context"
	"fmt"
	"strings"
	"testing"
	"time"

	external_response "github.com/chef/automate/api/external/cfgmgmt/response"
	"github.com/chef/automate/api/interservice/cfgmgmt/request"
	authzConstants "github.com/chef/automate/components/authz-service/constants/v2"
	iBackend "github.com/chef/automate/components/ingest-service/backend"
	"github.com/golang/protobuf/jsonpb"
	"github.com/golang/protobuf/proto"
	"github.com/golang/protobuf/ptypes"
	gp "github.com/golang/protobuf/ptypes/struct"
	"github.com/stretchr/testify/assert"
)

func TestNodesReturnsEmptyList(t *testing.T) {
	// rpc GetNodes (request.Nodes) returns (google.protobuf.ListValue)
	ctx := context.Background()
	req := request.Nodes{}

	expected := new(gp.ListValue)
	res, err := cfgmgmt.GetNodes(ctx, &req)

	assert.Nil(t, err)
	assert.Equal(t, expected, res)
}

func TestNodesWithFilterReturnsListWithOneNode(t *testing.T) {
	nodes := twoNodeArray()

	suite.IngestNodes(nodes)
	defer suite.DeleteAllDocuments()

	ctx := context.Background()
	req := request.Nodes{}

	// Filtering by status:success
	req.Filter = []string{"status:success"}

	t.Run(fmt.Sprintf("with filter '%v' should return only one node", req.Filter),
		func(t *testing.T) {
			expectedNodeArray := []iBackend.Node{nodes[0]}
			expected := expectedNodeListFromNodeArray(expectedNodeArray)

			res, err := cfgmgmt.GetNodes(ctx, &req)
			assert.Nil(t, err)
			assert.Equal(t, expected, res)
		})

	// Filtering by status:missing
	req.Filter = []string{"status:missing"}

	t.Run(fmt.Sprintf("with filter '%v' should return only one node", req.Filter),
		func(t *testing.T) {
			expectedNodeArray := []iBackend.Node{nodes[1]}
			expected := expectedNodeListFromNodeArray(expectedNodeArray)

			res, err := cfgmgmt.GetNodes(ctx, &req)
			assert.Nil(t, err)
			assert.Equal(t, expected, res)
		})
}

func TestNodesWithPagination(t *testing.T) {
	nodes := twoNodeArray()

	suite.IngestNodes(nodes)
	defer suite.DeleteAllDocuments()

	ctx := context.Background()
	req := request.Nodes{
		Pagination: &request.Pagination{
			Size: 1,
		},
	}

	// Showing page number one (by default)
	t.Run(fmt.Sprintf("with pagination '%v' should return only one node/page", req.Pagination),
		func(t *testing.T) {
			expectedNodeArray := []iBackend.Node{nodes[0]}
			expected := expectedNodeListFromNodeArray(expectedNodeArray)

			res, err := cfgmgmt.GetNodes(ctx, &req)
			assert.Nil(t, err)
			assert.Equal(t, expected, res)
		})

	// Showing page number two
	req.Pagination.Page = 2

	t.Run(fmt.Sprintf("with pagination '%v' should return only one node/page", req.Pagination),
		func(t *testing.T) {
			expectedNodeArray := []iBackend.Node{nodes[1]}
			expected := expectedNodeListFromNodeArray(expectedNodeArray)

			res, err := cfgmgmt.GetNodes(ctx, &req)
			assert.Nil(t, err)
			assert.Equal(t, expected, res)
		})
}

func TestNodesWithSorting(t *testing.T) {
	nodes := twoNodeArray()

	suite.IngestNodes(nodes)
	defer suite.DeleteAllDocuments()

	ctx := context.Background()
	req := request.Nodes{
		Sorting: &request.Sorting{Field: "name"},
	}

	// Sorting (by default asc) by name
	t.Run(fmt.Sprintf("with sorting '%v' should sort the nodes ASC (default)", req.Sorting),
		func(t *testing.T) {
			expected := expectedNodeListFromNodeArray(nodes)

			res, err := cfgmgmt.GetNodes(ctx, &req)
			assert.Nil(t, err)
			assert.Equal(t, expected, res)
		})

	// Sorting DESC by name
	req.Sorting.Order = request.Order_desc

	t.Run(fmt.Sprintf("with sorting '%v' should sort the nodes DESC", req.Sorting),
		func(t *testing.T) {
			// Reverse order
			expectedNodeArray := []iBackend.Node{nodes[1], nodes[0]}
			expected := expectedNodeListFromNodeArray(expectedNodeArray)

			res, err := cfgmgmt.GetNodes(ctx, &req)
			assert.Nil(t, err)
			assert.Equal(t, expected, res)
		})
}

func TestNodesProjectFilter(t *testing.T) {

	cases := []struct {
		description string
		nodes       []iBackend.Node
		request     request.Nodes
		ctx         context.Context
		expected    []string
	}{
		{
			description: "Two nodes matching on the same project tag",
			nodes: []iBackend.Node{
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "1",
					},
					Projects: []string{"one"},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "2",
					},
					Projects: []string{"two", "one"},
				},
			},
			ctx:      contextWithProjects([]string{"one"}),
			request:  request.Nodes{},
			expected: []string{"1", "2"},
		},
		{
			description: "Two nodes matching with two project tags",
			nodes: []iBackend.Node{
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "1",
					},
					Projects: []string{"one"},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "2",
					},
					Projects: []string{"one"},
				},
			},
			ctx:      contextWithProjects([]string{"one", "two"}),
			request:  request.Nodes{},
			expected: []string{"1", "2"},
		},
		{
			description: "Two nodes, one matching",
			nodes: []iBackend.Node{
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "1",
					},
					Projects: []string{"three"},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "2",
					},
					Projects: []string{"two", "one"},
				},
			},
			ctx:      contextWithProjects([]string{"one"}),
			request:  request.Nodes{},
			expected: []string{"2"},
		},
		{
			description: "Matching all",
			nodes: []iBackend.Node{
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "1",
					},
					Projects: []string{"three"},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "2",
					},
					Projects: []string{"two", "one"},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "3",
					},
				},
			},
			ctx:      contextWithProjects([]string{authzConstants.AllProjectsExternalID}),
			request:  request.Nodes{},
			expected: []string{"1", "2", "3"},
		},
		{
			description: "Match one unassigned",
			nodes: []iBackend.Node{
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "1",
					},
					Projects: []string{},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "2",
					},
					Projects: []string{"two", "one"},
				},
			},
			ctx:      contextWithProjects([]string{authzConstants.UnassignedProjectID}),
			request:  request.Nodes{},
			expected: []string{"1"},
		},
		{
			description: "No unassigned; no matches",
			nodes: []iBackend.Node{
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "1",
					},
					Projects: []string{"one"},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "2",
					},
					Projects: []string{"two", "one"},
				},
			},
			ctx:      contextWithProjects([]string{authzConstants.UnassignedProjectID}),
			request:  request.Nodes{},
			expected: []string{},
		},
		{
			description: "Match one unassigned and one assigned",
			nodes: []iBackend.Node{
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "1",
					},
					Projects: []string{},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "2",
					},
					Projects: []string{"two"},
				},
			},
			ctx:      contextWithProjects([]string{authzConstants.UnassignedProjectID, "two"}),
			request:  request.Nodes{},
			expected: []string{"1", "2"},
		},
		{
			description: "Match all projects with status filter",
			nodes: []iBackend.Node{
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "1",
						Status:   "failure",
					},
					Projects: []string{},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "2",
						Status:   "success",
					},
					Projects: []string{"two"},
				},
			},
			ctx: contextWithProjects([]string{authzConstants.AllProjectsExternalID}),
			request: request.Nodes{
				Filter: []string{"status:success"},
			},
			expected: []string{"2"},
		},
		{
			description: "Match 'one' project with status filter",
			nodes: []iBackend.Node{
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "1",
						Status:   "failure",
					},
					Projects: []string{"one"},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "2",
						Status:   "success",
					},
					Projects: []string{"two"},
				},
			},
			ctx: contextWithProjects([]string{"one"}),
			request: request.Nodes{
				Filter: []string{"status:success"},
			},
			expected: []string{},
		},
		{
			description: "Match unassigned projects with status filter",
			nodes: []iBackend.Node{
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "1",
						Status:   "failure",
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "2",
						Status:   "success",
					},
					Projects: []string{},
				},
			},
			ctx: contextWithProjects([]string{authzConstants.UnassignedProjectID}),
			request: request.Nodes{
				Filter: []string{"status:success"},
			},
			expected: []string{"2"},
		},
	}

	for _, test := range cases {
		t.Run(fmt.Sprintf("Project filter: %s", test.description), func(t *testing.T) {

			// Adding required node data
			for index, _ := range test.nodes {
				test.nodes[index].Exists = true
				test.nodes[index].NodeInfo.EntityUuid = newUUID()
			}

			// Add node with project
			suite.IngestNodes(test.nodes)
			defer suite.DeleteAllDocuments()

			// call GetNodes
			res, err := cfgmgmt.GetNodes(test.ctx, &test.request)
			assert.NoError(t, err)

			names := getNames(res)

			// Test what nodes are returned.
			assert.ElementsMatch(t, test.expected, names)
		})
	}
}

func TestNodesWithTableDriven(t *testing.T) {
	dataNodes := []struct {
		number int
		node   iBackend.NodeInfo
	}{
		{10, iBackend.NodeInfo{
			Status: "success", Platform: "ubuntu",
			Environment: "dev", OrganizationName: "org1"}},
		{10, iBackend.NodeInfo{
			Status: "success", Platform: "windows",
			Environment: "dev", OrganizationName: "org2"}},
		{10, iBackend.NodeInfo{
			Status: "failure", Platform: "ubuntu",
			Environment: "prod", OrganizationName: "org3"}},
		{20, iBackend.NodeInfo{
			Status: "success", Platform: "centos",
			Environment: "prod", OrganizationName: "org1"}},
		{10, iBackend.NodeInfo{
			Status: "failure", Platform: "oracle",
			Environment: "dev", OrganizationName: "org2"}},
		{20, iBackend.NodeInfo{
			Status: "missing", Platform: "ubuntu",
			Environment: "prod", OrganizationName: "org3"}},
	}

	var (
		nodes      = make([]iBackend.Node, 0)
		namePrefix = "node-"
		index      = 0
	)

	for _, data := range dataNodes {
		for i := 0; i < data.number; i++ {
			data.node.EntityUuid = newUUID()
			data.node.NodeName = namePrefix + fmt.Sprintf("%03d", index)
			node := iBackend.Node{
				NodeInfo: data.node,
				Exists:   true,
			}
			nodes = append(nodes, node)
			index++
		}
	}
	suite.IngestNodes(nodes)
	defer suite.DeleteAllDocuments()

	ctx := context.Background()
	cases := []struct {
		description string
		request     request.Nodes
		expected    *gp.ListValue
	}{
		{"should return first 10 nodes (default)",
			request.Nodes{},
			expectedNodeListFromNodeArray(nodes[0:10])},
		{"should return all nodes",
			request.Nodes{Pagination: &request.Pagination{Size: int32(index)}},
			expectedNodeListFromNodeArray(nodes)},
		{"should return all 'ubuntu' nodes",
			request.Nodes{
				Filter: []string{"platform:ubuntu"},
				Pagination: &request.Pagination{
					Size: int32(len(filterNodesArray(nodes, "platform:ubuntu")) + 1)},
			},
			expectedNodeListFromNodeArray(filterNodesArray(nodes, "platform:ubuntu"))},
		{"should return all 'ubuntu' nodes with wildcard",
			request.Nodes{
				Filter: []string{"platform:ubu?tu"},
				Pagination: &request.Pagination{
					Size: int32(len(filterNodesArray(nodes, "platform:ubuntu")) + 1)},
			},
			expectedNodeListFromNodeArray(filterNodesArray(nodes, "platform:ubuntu"))},
		{"should return all nodes when wildcard org search",
			request.Nodes{
				Filter:     []string{"organization:o*"},
				Pagination: &request.Pagination{Size: int32(index)},
			},
			expectedNodeListFromNodeArray(nodes)},
		{"should return all prod nodes when combined wildcard org and env prod search",
			request.Nodes{
				Filter:     []string{"organization:o*", "environment:prod"},
				Pagination: &request.Pagination{Size: int32(len(filterNodesArray(nodes, "environment:prod")) + 1)},
			},
			expectedNodeListFromNodeArray(filterNodesArray(nodes, "environment:prod"))},
		{"should return all prod nodes when combined wildcard o* and env pro? wildcard search",
			request.Nodes{
				Filter:     []string{"organization:o*", "environment:pro?"},
				Pagination: &request.Pagination{Size: int32(len(filterNodesArray(nodes, "environment:prod")) + 1)},
			},
			expectedNodeListFromNodeArray(filterNodesArray(nodes, "environment:prod"))},
		{"should return all nodes that are marked as 'success'",
			request.Nodes{
				Filter: []string{"status:success"},
				Pagination: &request.Pagination{
					Size: int32(len(filterNodesArray(nodes, "status:success")) + 1)},
			},
			expectedNodeListFromNodeArray(filterNodesArray(nodes, "status:success"))},
		{"should return only nodes from organization org2 ",
			request.Nodes{
				Filter: []string{"organization:org2"},
				Pagination: &request.Pagination{
					Size: int32(len(filterNodesArray(nodes, "organization:org2")) + 1)},
			},
			expectedNodeListFromNodeArray(filterNodesArray(nodes, "organization:org2"))},
		{"should return nodes from both dev & prod environments ",
			request.Nodes{
				Filter: []string{"environment:dev", "environment:prod"},
				Pagination: &request.Pagination{
					Size: int32(index + 1)},
			},
			expectedNodeListFromNodeArray(nodes)},
		{"should return only node-010",
			request.Nodes{Filter: []string{"name:node-010"}},
			expectedNodeListFromNodeArray(filterNodesArray(nodes, "name:node-010"))},
		{"should return NO nodes",
			request.Nodes{Filter: []string{"name:NOT_FOUND"}},
			new(gp.ListValue)},
		{"should return NO nodes (No missing windows nodes)",
			request.Nodes{Filter: []string{"status:missing", "platform:windows"}},
			new(gp.ListValue)},
		{"should return page 1 for 20 nodes from the prod environment",
			request.Nodes{
				Filter: []string{"environment:prod"},
				Pagination: &request.Pagination{
					Page: 1,
					Size: int32(20)},
			},
			expectedNodeListFromNodeArray(filterNodesArray(nodes, "environment:prod")[0:20])},
		{"should return page 2 for 20 nodes from the prod environment",
			request.Nodes{
				Filter: []string{"environment:prod"},
				Pagination: &request.Pagination{
					Page: 2,
					Size: int32(20)},
			},
			expectedNodeListFromNodeArray(filterNodesArray(nodes, "environment:prod")[20:40])},
		{"should return the list of all nodes sort by 'name' ASC",
			request.Nodes{
				Sorting: &request.Sorting{
					Field: "name",
					Order: request.Order_asc,
				},
				Pagination: &request.Pagination{Size: int32(index)},
			},
			expectedNodeListFromNodeArray(nodes)},
		{"should return the list of all nodes sort by 'name' DESC",
			request.Nodes{
				Sorting: &request.Sorting{
					Field: "name",
					Order: request.Order_desc,
				},
				Pagination: &request.Pagination{Size: int32(index)},
			},
			expectedNodeListFromNodeArray(reverseNodesArray(nodes))},
		{"should return the list of successful nodes sort by 'name' DESC",
			request.Nodes{
				Filter: []string{"status:success"},
				Sorting: &request.Sorting{
					Field: "name",
					Order: request.Order_desc,
				},
				Pagination: &request.Pagination{Size: int32(index)},
			},
			expectedNodeListFromNodeArray(reverseNodesArray(filterNodesArray(nodes, "status:success")))},
	}

	// Run all the cases!
	for _, test := range cases {
		t.Run(fmt.Sprintf("with request '%v' it %s", test.request, test.description),
			func(t *testing.T) {
				res, err := cfgmgmt.GetNodes(ctx, &test.request)
				assert.NoError(t, err)
				assert.Equal(t, test.expected, res)
			})
	}
}

// Node's checkin time is before last converge-history index
func TestNodesMissingRunsBeforeIndex(t *testing.T) {
	uuid := newUUID()
	// create a run with time now
	run := iBackend.Run{
		NodeInfo: iBackend.NodeInfo{
			EntityUuid: uuid,
			Status:     "success",
		},
		RunID:     newUUID(),
		StartTime: time.Now().AddDate(0, 0, -1), // started a day
		EndTime:   time.Now(),                   // the run just finished
	}

	suite.IngestRuns([]iBackend.Run{run})

	// create a node with the checkin time before the run time
	node := iBackend.Node{
		Checkin:     time.Now().AddDate(-1, 0, 0), // a checkin time for a year ago
		Exists:      true,
		LatestRunID: "f97039e0-3f2f-11e8-b467-0ed5f89f718b",
		NodeInfo: iBackend.NodeInfo{
			EntityUuid:       uuid,
			NodeName:         "node_name",
			Status:           "success",
			Platform:         "ubuntu",
			Environment:      "dev",
			OrganizationName: "org1",
		},
	}

	suite.IngestNodes([]iBackend.Node{node})
	defer suite.DeleteAllDocuments()

	res, err := cfgmgmt.GetNodes(context.Background(), &request.Nodes{})

	assert.Nil(t, err)

	// test that the node has a 'HasRunsData' == false
	assert.Equal(t, 1, len(res.Values))
	if len(res.Values) > 0 {
		structValue := res.Values[0].GetStructValue()
		value, ok := structValue.Fields["has_runs_data"]
		assert.True(t, !ok || !value.GetBoolValue(),
			"'has_runs_data' should be missing or set to false")
	}
}

// LastCCRReceived is before last index, checkin time after
// This could be the case for a liveness managed node
func TestLastCCRBeforeIndex(t *testing.T) {
	uuid := newUUID()
	// create a run with time now
	run := iBackend.Run{
		NodeInfo: iBackend.NodeInfo{
			EntityUuid: uuid,
			Status:     "success",
		},
		RunID:     newUUID(),
		StartTime: time.Now().AddDate(0, 0, -1), // started a day
		EndTime:   time.Now(),                   // the run just finished
	}

	suite.IngestRuns([]iBackend.Run{run})

	// create a node with the checkin time before the run time
	node := iBackend.Node{
		Checkin:         time.Now(),
		Exists:          true,
		LatestRunID:     "f97039e0-3f2f-11e8-b467-0ed5f89f718b",
		LastCCRReceived: time.Now().AddDate(-1, 0, 0), // a year ago
		NodeInfo: iBackend.NodeInfo{
			EntityUuid:       uuid,
			NodeName:         "node_name",
			Status:           "success",
			Platform:         "ubuntu",
			Environment:      "dev",
			OrganizationName: "org1",
		},
	}

	suite.IngestNodes([]iBackend.Node{node})
	defer suite.DeleteAllDocuments()

	res, err := cfgmgmt.GetNodes(context.Background(), &request.Nodes{})

	assert.Nil(t, err)

	// test that the node has a 'HasRunsData' == false
	assert.Equal(t, 1, len(res.Values))
	if len(res.Values) > 0 {
		structValue := res.Values[0].GetStructValue()
		value, ok := structValue.Fields["has_runs_data"]
		assert.True(t, !ok || !value.GetBoolValue(),
			"'has_runs_data' should be missing or set to false")
	}
}

// Node's checkin time is after last converge-history index
func TestNodesMissingRunsAfterIndex(t *testing.T) {
	uuid := newUUID()

	// create a run
	run := iBackend.Run{
		NodeInfo: iBackend.NodeInfo{
			EntityUuid: uuid,
			Status:     "success",
		},
		RunID:     newUUID(),
		StartTime: time.Now().AddDate(0, -1, -1), // started a month and a day ago
		EndTime:   time.Now().AddDate(0, -1, -1), // finished a month ago
	}

	suite.IngestRuns([]iBackend.Run{run})

	// create a node with the checkin time after the run time
	node := iBackend.Node{
		Checkin:     time.Now(), // a checkin time is now
		Exists:      true,
		LatestRunID: "f97039e0-3f2f-11e8-b467-0ed5f89f718b",
		NodeInfo: iBackend.NodeInfo{
			EntityUuid:       uuid,
			NodeName:         "node_name",
			Status:           "success",
			Platform:         "ubuntu",
			Environment:      "dev",
			OrganizationName: "org1",
		},
	}

	suite.IngestNodes([]iBackend.Node{node})
	defer suite.DeleteAllDocuments()

	res, err := cfgmgmt.GetNodes(context.Background(), &request.Nodes{})

	assert.Nil(t, err)

	// test that the node has a 'HasRunsData' == true
	assert.Equal(t, 1, len(res.Values), "There should only be one node")
	if len(res.Values) > 0 {
		structValue := res.Values[0].GetStructValue()
		value, ok := structValue.Fields["has_runs_data"]
		assert.True(t, ok && value.GetBoolValue(),
			"'has_runs_data' should not be missing and set to true")
	}
}

// converge-history index is 'converge-history-0001.01.01' the nodes with dates
// after this should have has_runs_data set to true
func TestNodesMissingRunsFromYear1(t *testing.T) {
	uuid := newUUID()

	indexDate := time.Date(1, 1, 1, 1, 1, 1, 1, time.UTC)

	// create a run
	run := iBackend.Run{
		NodeInfo: iBackend.NodeInfo{
			EntityUuid: uuid,
			Status:     "success",
		},
		RunID:     newUUID(),
		StartTime: indexDate.Add(-1 / time.Minute),
		EndTime:   indexDate,
	}

	suite.IngestRuns([]iBackend.Run{run})

	// create a node with the checkin time of now
	node := iBackend.Node{
		Checkin:     time.Now(),
		Exists:      true,
		LatestRunID: "f97039e0-3f2f-11e8-b467-0ed5f89f718b",
		NodeInfo: iBackend.NodeInfo{
			EntityUuid:       uuid,
			NodeName:         "node_name",
			Status:           "success",
			Platform:         "ubuntu",
			Environment:      "dev",
			OrganizationName: "org1",
		},
	}

	suite.IngestNodes([]iBackend.Node{node})
	defer suite.DeleteAllDocuments()

	res, err := cfgmgmt.GetNodes(context.Background(), &request.Nodes{})

	assert.NoError(t, err)

	// test that the node has a 'HasRunsData' == true
	assert.Equal(t, 1, len(res.Values), "There should only be one node")
	if len(res.Values) > 0 {
		structValue := res.Values[0].GetStructValue()
		value, ok := structValue.Fields["has_runs_data"]
		assert.True(t, ok, "'has_runs_data' should not be missing")
		assert.True(t, value.GetBoolValue(), "'has_runs_data' should be set to true")
	}
}

// Node's checkin time the beginning of the day the same date as the last converge-history index
func TestNodesMissingRunsEqualIndex(t *testing.T) {
	uuid := newUUID()

	indexDate := time.Now().UTC()

	// create a run
	run := iBackend.Run{
		NodeInfo: iBackend.NodeInfo{
			EntityUuid: uuid,
			Status:     "success",
		},
		RunID:     newUUID(),
		StartTime: indexDate.Add(-1 / time.Minute), // started a minute before now
		EndTime:   indexDate,                       // ended now
	}

	suite.IngestRuns([]iBackend.Run{run})

	// create a node with the checkin time the same day and at the beginning of the day the run time
	beginningOfTheDay := time.Date(indexDate.Year(), indexDate.Month(), indexDate.Day(),
		0, 0, 0, 0, indexDate.Location())
	node := iBackend.Node{
		Checkin:     beginningOfTheDay,
		Exists:      true,
		LatestRunID: "f97039e0-3f2f-11e8-b467-0ed5f89f718b",
		NodeInfo: iBackend.NodeInfo{
			EntityUuid:       uuid,
			NodeName:         "node_name",
			Status:           "success",
			Platform:         "ubuntu",
			Environment:      "dev",
			OrganizationName: "org1",
		},
	}

	suite.IngestNodes([]iBackend.Node{node})
	defer suite.DeleteAllDocuments()

	res, err := cfgmgmt.GetNodes(context.Background(), &request.Nodes{})

	assert.Nil(t, err)

	// test that the node has a 'HasRunsData' == true
	assert.Equal(t, 1, len(res.Values), "There should only be one node")
	if len(res.Values) > 0 {
		structValue := res.Values[0].GetStructValue()
		value, ok := structValue.Fields["has_runs_data"]
		assert.True(t, ok && value.GetBoolValue(),
			"'has_runs_data' should not be missing and set to true")
	}
}

// Node's checkin time is after last converge-history index, but the 'LatestRunID'
// is set to empty so 'HasRunsData' should equal false
func TestNodesMissingRunsLatestRunIDEmpty(t *testing.T) {
	uuid := newUUID()

	// create a run
	run := iBackend.Run{
		NodeInfo: iBackend.NodeInfo{
			EntityUuid: uuid,
			Status:     "success",
		},
		RunID:     newUUID(),
		StartTime: time.Now().AddDate(0, -1, -1), // started a month and a day ago
		EndTime:   time.Now().AddDate(0, -1, -1), // finished a month ago
	}

	suite.IngestRuns([]iBackend.Run{run})

	// create a node with the checkin time after the run time
	node := iBackend.Node{
		Checkin:     time.Now(), // a checkin time is now
		Exists:      true,
		LatestRunID: "",
		NodeInfo: iBackend.NodeInfo{
			EntityUuid:       uuid,
			NodeName:         "node_name",
			Status:           "success",
			Platform:         "ubuntu",
			Environment:      "dev",
			OrganizationName: "org1",
		},
	}

	suite.IngestNodes([]iBackend.Node{node})
	defer suite.DeleteAllDocuments()

	res, err := cfgmgmt.GetNodes(context.Background(), &request.Nodes{})

	assert.Nil(t, err)

	// test that the node has a 'HasRunsData' == false
	assert.Equal(t, 1, len(res.Values), "There should only be one node")
	if len(res.Values) > 0 {
		structValue := res.Values[0].GetStructValue()
		value, ok := structValue.Fields["has_runs_data"]
		assert.True(t, !ok || !value.GetBoolValue(),
			"'has_runs_data' should be missing or set to false")
	}
}

func ingestNodeArrayToMessageArray(nodes []iBackend.Node, hasRunsData bool) []proto.Message {
	messages := make([]proto.Message, len(nodes))
	for i, node := range nodes {
		checkin, _ := ptypes.TimestampProto(node.Checkin.UTC())
		messages[i] = &external_response.Node{
			Id:              node.EntityUuid,
			Name:            node.NodeName,
			Fqdn:            node.Fqdn,
			Checkin:         checkin,
			UptimeSeconds:   int32(node.UptimeSeconds),
			Organization:    node.OrganizationName,
			Environment:     node.Environment,
			Platform:        node.Platform,
			PlatformFamily:  node.PlatformFamily,
			PlatformVersion: node.PlatformVersion,
			Status:          node.Status,
			SourceFqdn:      node.SourceFqdn,
			LatestRunId:     node.LatestRunID,
			PolicyName:      node.PolicyName,
			PolicyGroup:     node.PolicyGroup,
			PolicyRevision:  node.PolicyRevision,
			HasRunsData:     hasRunsData,
			LastCcrReceived: checkin,
		}
	}
	return messages
}

func expectedNodeListFromNodeArray(nodes []iBackend.Node) *gp.ListValue {
	// default to hasRunsData == false because there is no converge-history indices
	return expectedNodeListFromNodeArrayWithHasRunsData(nodes, false)
}

func expectedNodeListFromNodeArrayWithHasRunsData(nodes []iBackend.Node, hasRunsData bool) *gp.ListValue {
	expectedList := new(gp.ListValue)
	msgArray := ingestNodeArrayToMessageArray(nodes, hasRunsData)
	// TODO: (@afiune) How do we wanna handle this error? panic¿?
	_ = messageArrayToListValue(msgArray, expectedList)
	return expectedList
}

// filterNodesArray will return a new list of Nodes that contains the provided filter
//
// Example of a filter:
// => "environment:prod"
// => "status:success"
// => "name:node-000"
func filterNodesArray(nodes []iBackend.Node, filter string) []iBackend.Node {
	var (
		filteredNodes = make([]iBackend.Node, 0)
		kv            = strings.Split(filter, ":")
		key           = kv[0]
		value         = kv[1]
	)

	for _, node := range nodes {
		if nodeFieldEqualToValue(node, key, value) {
			filteredNodes = append(filteredNodes, node)
		}
	}
	return filteredNodes
}

// nodeFieldEqualToValue checks if the provided Node matches with the provided Value
//
// Example: Does a Node belong to a specific environment? (prod)
// ```
// nodeFieldEqualToValue(node, "environment", "prod")
// ```
func nodeFieldEqualToValue(node iBackend.Node, key string, value string) bool {
	switch key {
	case "platform":
		return node.Platform == value
	case "environment":
		return node.Environment == value
	case "status":
		return node.Status == value
	case "name":
		return node.NodeName == value
	case "organization":
		return node.OrganizationName == value
	}
	return false
}

// twoNodeArray returns an Array of 2 Nodes
func twoNodeArray() []iBackend.Node {
	return []iBackend.Node{
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
}

// reverseNodesArray accepts a Nodes Array and reverses it
func reverseNodesArray(input []iBackend.Node) []iBackend.Node {
	if len(input) == 0 {
		return input
	}
	return append(reverseNodesArray(input[1:]), input[0])
}

// messageArrayToListValue Casts a 'Proto Message Array' into a 'Proto ListValue'
//
// Copied from 'grpcserver/util.go'
//
// Why? - We do not want to use private functions that we do not expose to consumers,
//        for that reason we are copy and pasting the functions inside the tests
func messageArrayToListValue(messages []proto.Message, list *gp.ListValue) error {
	list.Values = make([]*gp.Value, len(messages))

	for i, msg := range messages {
		v := gp.Value{}
		content, err := getMessageRawJSON(msg)
		if err != nil {
			return err
		}

		err = protoFromJSON(content, &v)
		if err != nil {
			return err
		}

		list.Values[i] = &v
	}
	return nil
}

func protoFromJSON(content string, pb proto.Message) error {
	unmarshaler := &jsonpb.Unmarshaler{AllowUnknownFields: true}
	return unmarshaler.Unmarshal(strings.NewReader(content), pb)
}

func getMessageRawJSON(message proto.Message) (string, error) {
	return (&jsonpb.Marshaler{OrigName: true}).MarshalToString(message)
}

func getNames(listValue *gp.ListValue) []string {
	names := make([]string, 0)
	for _, value := range listValue.GetValues() {
		m := value.GetStructValue().GetFields()
		names = append(names, m["name"].GetStringValue())
	}

	return names
}
