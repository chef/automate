package manager

import (
	"context"
	"testing"
	"time"

	"github.com/chef/a2/components/compliance-service/api/common"
	"github.com/chef/a2/components/compliance-service/api/jobs"
	"github.com/chef/a2/components/compliance-service/api/reporting"
	"github.com/chef/a2/components/nodemanager-service/api/manager"
	"github.com/chef/a2/components/nodemanager-service/api/nodes"
	"github.com/chef/a2/components/nodemanager-service/tests/mgrtesthelpers"
	"github.com/golang/protobuf/ptypes"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestGCPAPINodes(t *testing.T) {
	if !mgrtesthelpers.CheckForCreds("gcp") {
		t.Log("gcp credentials missing; aborting")
		t.FailNow()
	}
	t.Log("Running GCP-API search nodes test.")
	ctx := context.Background()

	cmpConn, err := mgrtesthelpers.GetComplianceConn()
	require.NoError(t, err)
	defer cmpConn.Close()

	mgrConn, err := mgrtesthelpers.GetManagerConn()
	require.NoError(t, err)
	defer mgrConn.Close()

	t.Log("connection to grpc successful")

	// setup clients
	mgrClient := manager.NewNodeManagerServiceClient(mgrConn)
	jobsClient := jobs.NewJobsServiceClient(cmpConn)
	reportingClient := reporting.NewReportingServiceClient(cmpConn)
	nodesClient := nodes.NewNodesServiceClient(mgrConn)

	// timestamp of now
	now := time.Now()
	originalReportingNodes, err := reportingClient.ListNodes(ctx, &reporting.Query{})
	require.NoError(t, err)
	t.Logf("Starting test at %s with %d nodes found in reporting", now, originalReportingNodes.GetTotal())

	// delete all existing gcp-api managers, just in case
	err = mgrtesthelpers.DeleteAllManagersByType(ctx, mgrClient, "gcp-api")
	require.NoError(t, err)

	// create nodemanager
	t.Log("Creating gcp-api node manager.")
	mgrID, err := mgrtesthelpers.AddGCPManager(ctx, mgrClient, "gcp-api")
	require.NoError(t, err)
	require.NotZero(t, len(mgrID.Ids))

	mgrIDVal := mgrID.GetIds()[0].Id

	t.Log("ensure a node was added")
	query := nodes.Query{
		Filters: []*common.Filter{
			{Key: "manager_id", Values: []string{mgrIDVal}},
		},
	}
	list, err := nodesClient.List(ctx, &query)
	require.NoError(t, err)
	assert.Equal(t, int32(1), list.GetTotal())

	// create a job with node manager reference
	mgrFilter := jobs.ManagerFilter{
		ManagerId: mgrID.GetIds()[0].Id,
		Filters:   []*common.Filter{},
	}
	job := jobs.Job{
		Name:          "my job for gcp-api node manager",
		Tags:          []*common.Kv{},
		Type:          "exec",
		Profiles:      []string{"https://s3.eu-west-2.amazonaws.com/apop-bucket/mygcp-0.3.0.tar.gz"},
		NodeSelectors: []*jobs.ManagerFilter{&mgrFilter},
	}
	t.Log("Creating job for gcp-api node manager, to execute scan job")
	jobID, err := jobsClient.Create(ctx, &job)
	require.NoError(t, err)

	// read the job to get the status, loop until completed. fail test if failed.
	jobRead, err := jobsClient.Read(ctx, jobID)
	require.NoError(t, err)

	status := jobRead.GetStatus()
	t.Log("Reading job status, looping until status reports as completed.")

	counter := 0
	for status != "completed" {
		t.Logf("status: %s (sleeping 1s)", status)
		time.Sleep(1 * time.Second)
		jobRead, err := jobsClient.Read(ctx, jobID)
		require.NoError(t, err)
		status = jobRead.Status
		if status == "failed" {
			t.Fatalf("job failed. job: %+v", jobRead)
		}
		counter++
		if counter > 120 {
			t.Fatalf("timed out waiting for job to finish")
		}
	}
	// ensure the job
	require.Equal(t, "completed", status)

	// check reporting nodes. if job completed we should have an extra node in reporting nodes
	reportingNodes, err := reportingClient.ListNodes(ctx, &reporting.Query{})
	require.NoError(t, err)

	// sometimes it takes a bit of extra time for the report to land in elastic, so here
	// we loop until it has landed
	counter = 0
	for reportingNodes.GetTotal() == originalReportingNodes.GetTotal() {
		t.Log("sleeping 1s; then retrieving reporting nodes total again")
		time.Sleep(1 * time.Second)
		reportingNodes, err = reportingClient.ListNodes(ctx, &reporting.Query{})
		require.NoError(t, err)
		counter++
		if counter > 120 {
			t.Fatalf("timed out waiting for report to land")
		}
	}
	require.Truef(t, reportingNodes.GetTotal() > originalReportingNodes.GetTotal(),
		"expected reportingNodes.Total (%d) > originalReportingNodes.Total (%d)",
		reportingNodes.GetTotal(), originalReportingNodes.GetTotal())

	for _, listNode := range reportingNodes.GetNodes() {
		endtime, err := ptypes.Timestamp(listNode.GetLatestReport().GetEndTime())
		require.NoError(t, err)
		if endtime.After(now) && listNode.GetEnvironment() == "gcp-api" {
			t.Logf("Beginning test time: %s", now)
			t.Logf("Found node %s, end time: %s", listNode, endtime)
			// check `/nodes` endpoint to ensure node marked as reachable
			foundNode, err := nodesClient.Read(ctx, &nodes.Id{Id: listNode.Id})
			require.NoError(t, err)
			require.Equal(t, "reachable", foundNode.GetStatus())
		}
	}
}
