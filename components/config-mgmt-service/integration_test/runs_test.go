//
//  Author:: Salim Afiune <afiune@chef.io>
//  Copyright:: Copyright 2017, Chef Software Inc.
//

package integration_test

import (
	"context"
	"fmt"
	"strings"
	"testing"
	"time"

	"google.golang.org/grpc/codes"

	"github.com/golang/protobuf/proto"
	gp "github.com/golang/protobuf/ptypes/struct"
	"github.com/stretchr/testify/assert"

	"github.com/chef/automate/api/interservice/cfgmgmt/request"
	"github.com/chef/automate/api/interservice/cfgmgmt/response"
	iBackend "github.com/chef/automate/components/ingest-service/backend"
	"github.com/chef/automate/lib/grpc/grpctest"
)

func TestRunsEmptyRequestReturnsError(t *testing.T) {
	ctx := context.Background()
	req := request.Runs{}

	expected := new(gp.ListValue)
	res, err := cfgmgmt.GetRuns(ctx, &req)
	grpctest.AssertCode(t, codes.InvalidArgument, err)
	assert.Equal(t, expected, res)
}

func TestRunsReturnErrorWithWrongParameters(t *testing.T) {
	var (
		ctx      = context.Background()
		expected = new(gp.ListValue)
	)

	cases := []request.Runs{
		{NodeId: "fake", Filter: []string{"platform=centos"}},
		{NodeId: "fake", Filter: []string{"wrong"}},
		{NodeId: "fake", Filter: []string{":success"}},
		{NodeId: "fake", Filter: []string{"platform:"}},
		{NodeId: "fake", Filter: []string{"platform:foo:bar"}},
		{NodeId: "fake", Start: "2000-00-00"},
		{NodeId: "fake", Start: "00-00-00"},
		{NodeId: "fake", Start: "18-10-10"},
		{NodeId: "fake", Start: "20-01-01"},
		{NodeId: "fake", Start: "17:01:01"},
		{NodeId: "fake", End: "01-01-1800"},
		{NodeId: "fake", End: "3000-12"},
		{NodeId: "fake", End: "2019"},
		{NodeId: "fake", End: "1888:01:01"},
		{NodeId: "fake", End: "2027/01/01"},
	}

	for _, test := range cases {
		t.Run(fmt.Sprintf("with parameters filters=%v start=%s end=%s it should return an error",
			test.GetFilter(), test.GetStart(), test.GetEnd()), func(t *testing.T) {
			res, err := cfgmgmt.GetRuns(ctx, &test)
			grpctest.AssertCode(t, codes.InvalidArgument, err)
			assert.Equal(t, expected, res)
		})
	}
}

func TestRunsWithANodeNotFoundReturnsError(t *testing.T) {
	ctx := context.Background()
	req := request.Runs{NodeId: "FAKE"}

	expected := new(gp.ListValue)
	res, err := cfgmgmt.GetRuns(ctx, &req)
	assert.NoError(t, err)
	assert.Equal(t, expected, res)
}
func TestRunsWithFilterReturnsListWithOneNode(t *testing.T) {
	var (
		nodeID = newUUID()
		runs   = twoRunsArray(nodeID)
		req    = request.Runs{NodeId: nodeID}
		ctx    = context.Background()
		node   = iBackend.Node{
			Exists: true,
			NodeInfo: iBackend.NodeInfo{
				EntityUuid: nodeID,
			},
		}
	)

	suite.IngestNodes([]iBackend.Node{node})
	suite.IngestRuns(runs)
	defer suite.DeleteAllDocuments()

	// Filtering by status:success
	req.Filter = []string{"status:success"}

	t.Run(fmt.Sprintf("with filter '%v' should return only one run", req.Filter),
		func(t *testing.T) {
			expectedRunArray := []iBackend.Run{runs[0]}
			expected := valueListFromRunArray(expectedRunArray)

			res, err := cfgmgmt.GetRuns(ctx, &req)
			assert.Nil(t, err)
			assert.Equal(t, expected, res)
		})

	// Filtering by status:failure
	req.Filter = []string{"status:failure"}

	t.Run(fmt.Sprintf("with filter '%v' should return only one run", req.Filter),
		func(t *testing.T) {
			expectedRunArray := []iBackend.Run{runs[1]}
			expected := valueListFromRunArray(expectedRunArray)

			res, err := cfgmgmt.GetRuns(ctx, &req)
			assert.Nil(t, err)
			assert.Equal(t, expected, res)
		})
}

func TestRunsWithPagination(t *testing.T) {
	var (
		nodeID = newUUID()
		runs   = twoRunsArray(nodeID)
		req    = request.Runs{NodeId: nodeID}
		ctx    = context.Background()
		node   = iBackend.Node{
			Exists: true,
			NodeInfo: iBackend.NodeInfo{
				EntityUuid: nodeID,
			},
		}
	)

	suite.IngestNodes([]iBackend.Node{node})
	suite.IngestRuns(runs)
	defer suite.DeleteAllDocuments()

	// Setting up pagination size to 1
	req.Pagination = &request.Pagination{Size: 1}

	// Showing page number one (by default)
	t.Run(fmt.Sprintf("with pagination '%v' should return only one run/page", req.Pagination),
		func(t *testing.T) {
			expectedRunArray := []iBackend.Run{runs[0]}
			expected := valueListFromRunArray(expectedRunArray)

			res, err := cfgmgmt.GetRuns(ctx, &req)
			assert.Nil(t, err)
			assert.Equal(t, expected, res)
		})

	// Showing page number two
	req.Pagination.Page = 2

	t.Run(fmt.Sprintf("with pagination '%v' should return only one run/page", req.Pagination),
		func(t *testing.T) {
			expectedRunArray := []iBackend.Run{runs[1]}
			expected := valueListFromRunArray(expectedRunArray)

			res, err := cfgmgmt.GetRuns(ctx, &req)
			assert.Nil(t, err)
			assert.Equal(t, expected, res)
		})
}

func TestRunsWithTableDriven(t *testing.T) {
	type table struct {
		nodeID string
		runs   []iBackend.Run
	}

	var (
		ctx           = context.Background()
		totalN        = 5            // total of nodes to add with a number of runs
		totalR        = 28           // total of runs to add per node (Needs an even number to divide/2)
		nodes         = []table{}    // A table of nodes win runs
		ymdLayout     = "2006-01-02" // 'YYYY-MM-DD' layout
		defaultLayout = time.RFC3339 // Default layout
		startTime     = time.Now()
		endTime       = time.Now().Add(time.Minute)
		formatTime    = func(t time.Time, d int, f string) string {
			return t.AddDate(0, 0, d).Format(f)
		}
		nodeArray = func(id string) []iBackend.Node {
			return []iBackend.Node{
				{
					NodeInfo: iBackend.NodeInfo{EntityUuid: id},
					Exists:   true},
			}
		}
	)

	for i := 0; i < totalN; i++ {
		var (
			id     = newUUID()
			runs   = make([]iBackend.Run, totalR)
			status = "success"
			count  = 0
		)

		for j := 0; j < totalR; j++ {
			runs[j] = newIngestRun(id, status,
				formatTime(startTime, j, defaultLayout),
				formatTime(endTime, j, defaultLayout))

			if count == 4 {
				count = 0
				if status == "success" {
					status = "failure"
				} else {
					status = "success"
				}
			} else {
				count++
			}
		}

		// We've got to reverse the order since we are always sorting DESC
		data := table{id, reverseRunsArray(runs)}
		nodes = append(nodes, data)
	}

	for _, node := range nodes {
		suite.IngestNodes(nodeArray(node.nodeID))
		suite.IngestRuns(node.runs)
	}
	defer suite.DeleteAllDocuments()

	cases := []struct {
		description string
		request     request.Runs
		expected    *gp.ListValue
	}{
		{"should return first 10 runs (default)",
			request.Runs{NodeId: nodes[0].nodeID},
			valueListFromRunArray(nodes[0].runs[0:10])},
		{"should return all runs",
			request.Runs{
				NodeId:     nodes[1].nodeID,
				Pagination: &request.Pagination{Size: int32(len(nodes[1].runs) + 1)}},
			valueListFromRunArray(nodes[1].runs)},
		{"should return all 'success' runs",
			request.Runs{
				NodeId: nodes[0].nodeID,
				Filter: []string{"status:success"},
				Pagination: &request.Pagination{
					Size: int32(len(filterRunsArray(nodes[0].runs, "status:success")) + 1)},
			},
			valueListFromRunArray(filterRunsArray(nodes[0].runs, "status:success"))},
		{"should return all runs that finished with a 'failure'",
			request.Runs{
				NodeId: nodes[2].nodeID,
				Filter: []string{"status:failure"},
				Pagination: &request.Pagination{
					Size: int32(len(filterRunsArray(nodes[2].runs, "status:failure")) + 1)},
			},
			valueListFromRunArray(filterRunsArray(nodes[2].runs, "status:failure"))},
		{"should return page 1 for 5 runs",
			request.Runs{
				NodeId: nodes[3].nodeID,
				Pagination: &request.Pagination{
					Page: 1,
					Size: int32(5)},
			},
			valueListFromRunArray(nodes[3].runs[0:5])},
		{"should return page 2 for 5 runs",
			request.Runs{
				NodeId: nodes[3].nodeID,
				Pagination: &request.Pagination{
					Page: 2,
					Size: int32(5)},
			},
			valueListFromRunArray(nodes[3].runs[5:10])},
		{"should return page 1 for 2 successful runs",
			request.Runs{
				NodeId: nodes[4].nodeID,
				Filter: []string{"status:success"},
				Pagination: &request.Pagination{
					Page: 1,
					Size: int32(2)},
			},
			valueListFromRunArray(filterRunsArray(nodes[4].runs, "status:success")[0:2])},
		{"should return page 2 for 2 failed runs",
			request.Runs{
				NodeId: nodes[3].nodeID,
				Filter: []string{"status:failure"},
				Pagination: &request.Pagination{
					Page: 2,
					Size: int32(2)},
			},
			valueListFromRunArray(filterRunsArray(nodes[3].runs, "status:failure")[2:4])},
		{"should return NO runs",
			request.Runs{
				NodeId: nodes[2].nodeID,
				Filter: []string{"environment:FAKE"}},
			new(gp.ListValue)},
		{"should return NO runs that started at the provided 'start' day",
			request.Runs{
				NodeId: nodes[2].nodeID,
				Start:  formatTime(startTime, totalR, ymdLayout)},
			new(gp.ListValue)},
		{"should return the first runs that started at the provided 'start' day",
			request.Runs{
				NodeId: nodes[2].nodeID,
				Start:  formatTime(startTime, totalR-1, ymdLayout)},
			expectedAbridgedRunListFromRun(nodes[2].runs[0])},
		{"should return NO runs that finished at the provided 'end' day",
			request.Runs{
				NodeId: nodes[1].nodeID,
				End:    formatTime(endTime, -1, ymdLayout)},
			new(gp.ListValue)},
		{"should return the last runs that finished at the provided 'end' day",
			request.Runs{
				NodeId: nodes[1].nodeID,
				End:    formatTime(endTime, 0, ymdLayout)},
			expectedAbridgedRunListFromRun(nodes[1].runs[totalR-1])},
		{"should return a single run from the provided time rage",
			request.Runs{
				NodeId: nodes[4].nodeID,
				Start:  formatTime(endTime, totalR/2, ymdLayout),
				End:    formatTime(endTime, totalR/2, ymdLayout)},
			expectedAbridgedRunListFromRun(nodes[4].runs[(totalR/2)-1])},
		{"should return the runs from the provided time rage",
			request.Runs{
				NodeId: nodes[4].nodeID,
				Start:  formatTime(endTime, (totalR/2)-1, ymdLayout),
				End:    formatTime(endTime, (totalR/2)+1, ymdLayout)},
			valueListFromRunArray(nodes[4].runs[(totalR/2)-2 : (totalR/2)+1])}, // Three runs
	}

	// Run all the cases!
	for _, test := range cases {
		t.Run(fmt.Sprintf("with request '%v' it %s", test.request, test.description),
			func(t *testing.T) {
				res, err := cfgmgmt.GetRuns(ctx, &test.request)
				assert.Nil(t, err)
				assert.Equal(t, test.expected, res)
			})
	}
}

func ingestRunArrayToAbridgedMessageArray(runs ...iBackend.Run) []proto.Message {
	messages := make([]proto.Message, len(runs))
	for i, run := range runs {
		messages[i] = &response.AbridgedConverge{
			Id:        run.RunID,
			StartTime: run.StartTime.Format(time.RFC3339),
			EndTime:   run.EndTime.Format(time.RFC3339),
			Status:    run.Status,
		}
	}
	return messages
}

func expectedAbridgedRunListFromRun(run iBackend.Run) *gp.ListValue {
	expectedList := new(gp.ListValue)
	msgArray := ingestRunArrayToAbridgedMessageArray(run)
	_ = messageArrayToListValue(msgArray, expectedList)
	return expectedList
}

func valueListFromRunArray(runs []iBackend.Run) *gp.ListValue {
	expectedList := new(gp.ListValue)
	msgArray := ingestRunArrayToAbridgedMessageArray(runs...)
	_ = messageArrayToListValue(msgArray, expectedList)
	return expectedList
}

// filterRunsArray will return a new list of Runs that contains the provided filter
//
// Example of a filter:
// => "environment:prod"
// => "status:success"
// => "name:node-000"
func filterRunsArray(runs []iBackend.Run, filter string) []iBackend.Run {
	var (
		filteredNodes = make([]iBackend.Run, 0)
		kv            = strings.Split(filter, ":")
		key           = kv[0]
		value         = kv[1]
	)

	for _, run := range runs {
		if runFieldEqualToValue(run, key, value) {
			filteredNodes = append(filteredNodes, run)
		}
	}
	return filteredNodes
}

// runFieldEqualToValue checks if the provided Run matches with the provided Value
//
// Example: Does a Run finished successfully?
// ```
// runFieldEqualToValue(run, "status", "success")
// ```
func runFieldEqualToValue(run iBackend.Run, key string, value string) bool {
	switch key {
	case "platform":
		return run.Platform == value
	case "environment":
		return run.Environment == value
	case "status":
		return run.Status == value
	case "name":
		return run.NodeName == value
	case "organization":
		return run.OrganizationName == value
	}
	return false
}

// twoRunsArray returns an Array of 2 Runs
//
// The trick here is to order the list of runs by 'end_time' DESC since
// that is the default, that's why we add 60s to the first last run
func twoRunsArray(UUID string) []iBackend.Run {
	return []iBackend.Run{
		newIngestRun(UUID, "success",
			time.Now().Format(time.RFC3339),
			time.Now().Add(time.Second*60).Format(time.RFC3339)), // This will ensure this run is the first one always
		newIngestRun(UUID, "failure",
			time.Now().Format(time.RFC3339),
			time.Now().Add(time.Second).Format(time.RFC3339)),
	}
}

func newIngestRun(UUID, status, start, end string) iBackend.Run {
	layout := "2006-01-02T15:04:05Z"
	// TODO: (@afiune) figure out how ingest is doing the parsing of the start/end
	// time from a CCR run since the format is different depending on the parsing
	// mechanism, for now we will use this closure to get the same format
	getTimeFromString := func(s string) time.Time {
		t, _ := time.Parse(layout, s)
		return t
	}

	return iBackend.Run{
		NodeInfo:  iBackend.NodeInfo{EntityUuid: UUID, Status: status},
		RunID:     newUUID(),
		StartTime: getTimeFromString(start),
		EndTime:   getTimeFromString(end),
	}
}

// reverseRunsArray accepts a Runs Array and reverses it
func reverseRunsArray(input []iBackend.Run) []iBackend.Run {
	if len(input) == 0 {
		return input
	}
	return append(reverseRunsArray(input[1:]), input[0])
}
