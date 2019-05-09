//
//  Author:: Salim Afiune <afiune@chef.io>
//  Copyright:: Copyright 2017, Chef Software Inc.
//

package integration_test

import (
	"context"
	"fmt"
	"testing"
	"time"

	"github.com/stretchr/testify/assert"
	"google.golang.org/grpc/codes"

	"github.com/chef/automate/api/interservice/cfgmgmt/request"
	"github.com/chef/automate/api/interservice/cfgmgmt/response"
	iBackend "github.com/chef/automate/components/ingest-service/backend"
	"github.com/chef/automate/lib/grpc/grpctest"
)

// A very basic test that doesn't require data ingestion
func TestStatsRunsCountsEmpty(t *testing.T) {
	ctx := context.Background()
	req := request.RunsCounts{}

	_, err := cfgmgmt.GetRunsCounts(ctx, &req)
	assert.Error(t, err)
}

func TestStatsRunsCountsWithTwoRuns(t *testing.T) {
	nodeID := newUUID()
	// Generate the objects you want to ingest
	runs := []iBackend.Run{
		{
			NodeInfo: iBackend.NodeInfo{
				EntityUuid: nodeID,
				Status:     "success",
			},
			StartTime: time.Now(),
			EndTime:   time.Now().Add(10),
		},
		{
			NodeInfo: iBackend.NodeInfo{
				EntityUuid: nodeID,
				Status:     "failure",
			},
			StartTime: time.Now(),
			EndTime:   time.Now().Add(10),
		},
	}

	node := iBackend.Node{
		Exists: true,
		NodeInfo: iBackend.NodeInfo{
			EntityUuid: nodeID,
		},
	}

	// Ingest the runs, this will automatically refresh the indexes
	suite.IngestRuns(runs)
	suite.IngestNodes([]iBackend.Node{node})
	defer suite.DeleteAllDocuments()

	ctx := context.Background()
	req := request.RunsCounts{
		NodeId: nodeID,
	}
	expected := &response.RunsCounts{
		Total:   2,
		Failure: 1,
		Success: 1,
	}

	res, err := cfgmgmt.GetRunsCounts(ctx, &req)
	assert.Nil(t, err)
	assert.Equal(t, expected, res)
}

func TestStatsRunsCountsWithTwoRunsAndTwoNodes(t *testing.T) {
	nodeID1 := newUUID()
	nodeID2 := newUUID()
	// Generate the objects you want to ingest
	runs := []iBackend.Run{
		{
			NodeInfo: iBackend.NodeInfo{
				EntityUuid: nodeID1,
				Status:     "success",
			},
			StartTime: time.Now(),
			EndTime:   time.Now().Add(10),
		},
		{
			NodeInfo: iBackend.NodeInfo{
				EntityUuid: nodeID2,
				Status:     "failure",
			},
			StartTime: time.Now(),
			EndTime:   time.Now().Add(10),
		},
	}

	nodes := []iBackend.Node{
		{
			Exists: true,
			NodeInfo: iBackend.NodeInfo{
				EntityUuid: nodeID1,
			},
		},
		{
			Exists: true,
			NodeInfo: iBackend.NodeInfo{
				EntityUuid: nodeID2,
			},
		},
	}

	// Ingest the runs, this will automatically refresh the indexes
	suite.IngestRuns(runs)
	suite.IngestNodes(nodes)
	defer suite.DeleteAllDocuments()

	ctx := context.Background()
	req1 := request.RunsCounts{
		NodeId: nodeID1,
	}
	expected1 := &response.RunsCounts{
		Total:   1,
		Failure: 0,
		Success: 1,
	}

	res1, err := cfgmgmt.GetRunsCounts(ctx, &req1)
	assert.Nil(t, err)
	assert.Equal(t, expected1, res1)

	req2 := request.RunsCounts{
		NodeId: nodeID2,
	}
	expected2 := &response.RunsCounts{
		Total:   1,
		Failure: 1,
		Success: 0,
	}

	res2, err := cfgmgmt.GetRunsCounts(ctx, &req2)
	assert.Nil(t, err)
	assert.Equal(t, expected2, res2)
}

// When adding a node ID in the filter it should always return zero runs
// This is a permission issue. If the node ID added in the filter was an 'or' with the node ID
// passed as a field then the user could see runs that they do not have permissions for. The user
// can only see runs for the node ID requested.
func TestStatsRunsCountsWithTwoRunsAndTwoNodesWithFilter(t *testing.T) {
	nodeID1 := newUUID()
	nodeID2 := newUUID()
	// Generate the objects you want to ingest
	runs := []iBackend.Run{
		{
			NodeInfo: iBackend.NodeInfo{
				EntityUuid: nodeID1,
				Status:     "success",
			},
			StartTime: time.Now(),
			EndTime:   time.Now().Add(10),
		},
		{
			NodeInfo: iBackend.NodeInfo{
				EntityUuid: nodeID2,
				Status:     "failure",
			},
			StartTime: time.Now(),
			EndTime:   time.Now().Add(10),
		},
	}

	nodes := []iBackend.Node{
		{
			Exists: true,
			NodeInfo: iBackend.NodeInfo{
				EntityUuid: nodeID1,
			},
		},
		{
			Exists: true,
			NodeInfo: iBackend.NodeInfo{
				EntityUuid: nodeID2,
			},
		},
	}

	// Ingest the runs, this will automatically refresh the indexes
	suite.IngestRuns(runs)
	suite.IngestNodes(nodes)
	defer suite.DeleteAllDocuments()

	ctx := context.Background()
	req := request.RunsCounts{
		NodeId: nodeID1,
		Filter: []string{"node_id:" + nodeID2},
	}
	expected := &response.RunsCounts{
		Total:   0,
		Failure: 0,
		Success: 0,
	}

	res, err := cfgmgmt.GetRunsCounts(ctx, &req)
	assert.Nil(t, err)
	assert.Equal(t, expected, res)
}

func TestStatsRunsCountsWith20Runs(t *testing.T) {
	var nRuns = 20
	var runs = make([]iBackend.Run, nRuns)
	nodeID := newUUID()

	// Generate 20[nRuns] run objects to ingest
	for i := 0; i < nRuns; i++ {
		runs[i] = iBackend.Run{
			NodeInfo: iBackend.NodeInfo{
				EntityUuid: nodeID,
				Status:     "success",
				Platform:   "ubuntu",
			},
			StartTime: time.Now(),
			EndTime:   time.Now().Add(10),
		}
	}

	node := iBackend.Node{
		Exists: true,
		NodeInfo: iBackend.NodeInfo{
			EntityUuid: nodeID,
		},
	}

	// Ingest the runs
	suite.IngestRuns(runs)
	suite.IngestNodes([]iBackend.Node{node})

	// Clean the documents
	defer suite.DeleteAllDocuments()

	ctx := context.Background()
	req := request.RunsCounts{
		NodeId: nodeID,
	} // We will modify this request
	expected := &response.RunsCounts{
		Total:   int32(nRuns),
		Success: int32(nRuns),
	}

	t.Logf("\nwith NO filter\n -> it should return all %d Run(s)", nRuns)
	res, err := cfgmgmt.GetRunsCounts(ctx, &req)
	assert.Nil(t, err)
	assert.Equal(t, expected, res)

	// Adding filters
	req.Filter = []string{"platform:ubuntu", "status:success"}
	t.Logf("\nwith filter(s) %v\n -> it should return %d Run(s)", req.GetFilter(), nRuns)
	res, err = cfgmgmt.GetRunsCounts(ctx, &req)
	assert.Nil(t, err)
	assert.Equal(t, expected, res)

	req.Filter = []string{"platform:windows"}
	expected = &response.RunsCounts{}
	t.Logf("\nwith filter(s) %v\n -> it should return 0 Run(s)", req.GetFilter())
	res, err = cfgmgmt.GetRunsCounts(ctx, &req)
	assert.Nil(t, err)
	assert.Equal(t, expected, res)
}

// Table driven sub-test to cover all code paths
func TestStatsRunsCountsFilteringWithTableDriven(t *testing.T) {
	dataRuns := []struct {
		number int
		node   iBackend.NodeInfo
	}{
		{10, iBackend.NodeInfo{Status: "success", Platform: "ubuntu"}},  // day 10 [timePlus(0)]
		{10, iBackend.NodeInfo{Status: "success", Platform: "windows"}}, // day 11 [timePlus(1)]
		{10, iBackend.NodeInfo{Status: "failure", Platform: "ubuntu"}},  // day 12 [timePlus(2)]
		{20, iBackend.NodeInfo{Status: "success", Platform: "centos"}},  // day 13 [timePlus(3)]
		{10, iBackend.NodeInfo{Status: "failure", Platform: "oracle"}},  // day 14 [timePlus(4)]
		{1, iBackend.NodeInfo{NodeName: "mock", Status: "success"}},     // day 15 [timePlus(5)]
	}

	nodeID := newUUID()

	var (
		totalRuns int32 = 0
		runs            = make([]iBackend.Run, totalRuns)

		// For the 'start' and 'end' parameters, we will need to increment the
		// start time day for a set of runs so we can assert and have different data
		//
		// We will start at:
		// => 2017-11-10 00:00:00 +0000 UTC
		//
		// and increment the day +1
		year      = 2017
		monthType = time.November
		month     = int(monthType)
		day       = 10
		testTime  = time.Date(year, monthType, day, 0, 0, 0, 0, time.UTC)

		// This tiny anonymous function will help us format the time
		// by adding 'x' number of days to our start time
		timePlus = func(x int) string {
			return fmt.Sprintf("%d-%d-%d", year, month, day+x)
		}
	)

	for _, data := range dataRuns {
		for i := 0; i < data.number; i++ {
			data.node.EntityUuid = nodeID
			run := iBackend.Run{
				NodeInfo:  data.node,
				StartTime: testTime,
				EndTime:   testTime.Add(10),
			}
			runs = append(runs, run)
			totalRuns++
		}
		// Increment the test time by +1 day
		testTime = testTime.AddDate(0, 0, 1)
	}

	node := iBackend.Node{
		Exists: true,
		NodeInfo: iBackend.NodeInfo{
			EntityUuid: nodeID,
		},
	}

	suite.IngestRuns(runs)
	suite.IngestNodes([]iBackend.Node{node})
	defer suite.DeleteAllDocuments()

	ctx := context.Background()
	cases := []struct {
		description string
		request     request.RunsCounts
		expected    response.RunsCounts
	}{
		{"should return all runs",
			request.RunsCounts{},
			response.RunsCounts{Total: totalRuns, Success: 41, Failure: 20}},
		{"should return only 10 centos runs",
			request.RunsCounts{Filter: []string{"platform:centos"}},
			response.RunsCounts{Total: 20, Success: 20}},
		{"should return only 41 successful runs",
			request.RunsCounts{Filter: []string{"status:success"}},
			response.RunsCounts{Total: 41, Success: 41}},
		{"should return only 10 failed ubuntu runs",
			request.RunsCounts{
				Filter: []string{"status:failure", "platform:ubuntu"},
			},
			response.RunsCounts{Total: 10, Failure: 10}},
		{"should return 0 successful oracle runs",
			request.RunsCounts{
				Filter: []string{"status:success", "platform:oracle"},
			},
			response.RunsCounts{}},
		{"should return 0 redhat runs",
			request.RunsCounts{Filter: []string{"platform:redhat"}},
			response.RunsCounts{}},
		{"should return only the 'mock' run",
			request.RunsCounts{Filter: []string{"name:mock"}},
			response.RunsCounts{Total: 1, Success: 1}},
		{"should return only all runs",
			request.RunsCounts{Start: timePlus(0)},
			response.RunsCounts{Total: totalRuns, Success: 41, Failure: 20}},
		{"should return only 31 runs that started at " + timePlus(3),
			request.RunsCounts{Start: timePlus(3)},
			response.RunsCounts{Total: 31, Success: 21, Failure: 10}},
		{"should return only 1 run that started at " + timePlus(5),
			request.RunsCounts{Start: timePlus(5)},
			response.RunsCounts{Total: 1, Success: 1}},
		{"should return only 0 run that started at " + timePlus(10),
			request.RunsCounts{Start: timePlus(10)},
			response.RunsCounts{}},
		{"should return only 20 runs",
			request.RunsCounts{
				Start: timePlus(1),
				End:   timePlus(2)},
			response.RunsCounts{Total: 20, Success: 10, Failure: 10}},
		{"should return only 10 successful windows runs",
			request.RunsCounts{
				Filter: []string{"status:success", "platform:windows"},
				Start:  timePlus(1),
				End:    timePlus(2)},
			response.RunsCounts{Total: 10, Success: 10}},
		{"should return 0 runs. (no successful ubuntu runs)",
			request.RunsCounts{
				Filter: []string{"status:success", "platform:ubuntu"},
				Start:  timePlus(1),
				End:    timePlus(2)},
			response.RunsCounts{}},
	}

	// Run all the cases!
	for _, test := range cases {
		t.Logf("\nwith parameter(s) filter=%v start=%v end=%v\n -> it %s",
			test.request.GetFilter(), test.request.GetStart(),
			test.request.GetEnd(), test.description)
		test.request.NodeId = nodeID
		res, err := cfgmgmt.GetRunsCounts(ctx, &test.request)
		assert.Nil(t, err)
		assert.Equal(t, test.expected, *res)
	}
}

func TestStatsRunsCountsWrongParameters(t *testing.T) {
	ctx := context.Background()

	cases := []request.RunsCounts{
		{Filter: []string{"platform=centos"}},
		{Filter: []string{"wrong"}},
		{Filter: []string{":success"}},
		{Filter: []string{"platform:"}},
		{Filter: []string{"platform:foo:bar"}},
		{Start: "2000-00-00"},
		{Start: "00-00-00"},
		{Start: "18-10-10"},
		{Start: "20-01-01"},
		{Start: "17:01:01"},
		{End: "01-01-1800"},
		{End: "3000-12"},
		{End: "2019"},
		{End: "1888:01:01"},
		{End: "2027/01/01"},
	}

	for _, test := range cases {
		t.Run(fmt.Sprintf("with parameters filters=%v start=%s end=%s it should return an error",
			test.GetFilter(), test.GetStart(), test.GetEnd()), func(t *testing.T) {
			res, err := cfgmgmt.GetRunsCounts(ctx, &test)
			assert.NotNil(t, err)
			assert.Nil(t, res)
			grpctest.AssertCode(t, codes.InvalidArgument, err)
		})
	}
}
