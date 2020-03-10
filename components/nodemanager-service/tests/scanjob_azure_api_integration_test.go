package manager

import (
	"context"
	"os"
	"testing"
	"time"

	"github.com/chef/automate/api/interservice/compliance/common"
	"github.com/chef/automate/api/interservice/compliance/jobs"
	"github.com/chef/automate/api/interservice/compliance/reporting"
	"github.com/chef/automate/api/interservice/nodemanager/manager"
	"github.com/chef/automate/api/interservice/nodemanager/nodes"
	"github.com/chef/automate/components/nodemanager-service/tests/mgrtesthelpers"
	"github.com/golang/protobuf/ptypes"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/status"
)

func TestAzureAPIScanJob(t *testing.T) {
	if !mgrtesthelpers.CheckForCreds("azure") {
		t.Log("azure credentials missing; aborting")
		t.FailNow()
	}
	t.Log("Running Azure-API scan job test.")
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

	// delete all existing azure-api managers, just in case
	mgrsList, err := mgrClient.List(ctx, &manager.Query{})
	require.NoError(t, err)
	for _, mgr := range mgrsList.GetManagers() {
		if mgr.Type == "azure-api" {
			_, err = mgrClient.Delete(ctx, &manager.Id{Id: mgr.Id})
			assert.Contains(t, []codes.Code{codes.NotFound, codes.OK}, status.Convert(err).Code())
		}
	}

	// create nodemanager
	t.Log("Creating azure-api node manager using env creds.")
	noCredsAzureMgr := manager.NodeManager{
		Name: "my test azure api mgr",
		Type: "azure-api",
		CredentialData: []*common.Kv{
			{Key: "AZURE_CLIENT_ID", Value: os.Getenv("AZURE_CLIENT_ID")},
			{Key: "AZURE_CLIENT_SECRET", Value: os.Getenv("AZURE_CLIENT_SECRET")},
			{Key: "AZURE_TENANT_ID", Value: os.Getenv("AZURE_TENANT_ID")}},
	}
	mgrID, err := mgrClient.Create(ctx, &noCredsAzureMgr)
	require.NoError(t, err)
	require.NotZero(t, len(mgrID.Ids))

	// create a job with node manager reference
	mgrFilter := jobs.ManagerFilter{
		ManagerId: mgrID.GetIds()[0].Id,
		Filters:   []*common.Filter{},
	}
	job := jobs.Job{
		Name:          "my job for azure-api node manager",
		Tags:          []*common.Kv{},
		Type:          "exec",
		Profiles:      []string{"https://github.com/vjeffrey/try-azure-profile/archive/master.tar.gz"},
		NodeSelectors: []*jobs.ManagerFilter{&mgrFilter},
	}
	t.Log("Creating job for node manager, to execute azure api scan")
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
	// check reporting nodes. if job completed we should have a node in reporting nodes
	reportingNodes, err := reportingClient.ListNodes(ctx, &reporting.Query{})
	require.NoError(t, err)

	// sometimes it takes a bit of extra time for the report to land in elastic, so here
	// we loop until it has
	counter = 0
	for reportingNodes.GetTotal() == originalReportingNodes.GetTotal() {
		t.Log("sleeping 1s; then retrieving reporting nodes total again")
		time.Sleep(1 * time.Second)
		reportingNodes, err = reportingClient.ListNodes(ctx, &reporting.Query{})
		require.NoError(t, err)
		counter++
		if counter > 120 {
			t.Fatalf("timed out waiting for job to finish")
		}
	}

	require.Equal(t, reportingNodes.GetTotal() > originalReportingNodes.GetTotal(), true)
	for _, listNode := range reportingNodes.GetNodes() {
		endtime, err := ptypes.Timestamp(listNode.GetLatestReport().GetEndTime())
		require.NoError(t, err)
		if endtime.After(now) && listNode.GetEnvironment() == "azure-api" {
			t.Logf("Beginning test time: %s", now)
			t.Logf("Found node %s, end time: %s", listNode, endtime)
			// check `/nodes` endpoint to ensure node marked as reachable
			foundNode, err := nodesClient.Read(ctx, &nodes.Id{Id: listNode.Id})
			require.NoError(t, err)
			require.Equal(t, "reachable", foundNode.GetStatus())
		}
	}
}
