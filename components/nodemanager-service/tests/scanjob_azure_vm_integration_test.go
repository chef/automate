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
	"github.com/chef/automate/components/nodemanager-service/tests/mgrtesthelpers"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/status"
)

// TODO: SCAN JOB for normal azure-vm scan

func TestAzureVMCredentiallessScanJob(t *testing.T) {
	if !mgrtesthelpers.CheckForCreds("azure") {
		t.Log("azure credentials missing; aborting")
		t.FailNow()
	}
	t.Log("Running Azure-VM credentialless scan job test.")
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

	// timestamp of now
	now := time.Now()
	originalReportingNodes, err := reportingClient.ListNodes(ctx, &reporting.Query{})
	require.NoError(t, err)
	t.Logf("Starting test at %s with %d nodes found in reporting", now, originalReportingNodes.GetTotal())

	// delete all existing azure-vm managers, just in case
	mgrsList, err := mgrClient.List(ctx, &manager.Query{})
	require.NoError(t, err)
	for _, mgr := range mgrsList.GetManagers() {
		if mgr.Type == "azure-vm" {
			_, err = mgrClient.Delete(ctx, &manager.Id{Id: mgr.Id})
			assert.Contains(t, []codes.Code{codes.NotFound, codes.OK}, status.Convert(err).Code())
		}
	}

	// create nodemanager
	t.Log("Creating azure-vm node manager using env creds.")
	noCredsAzureMgr := manager.NodeManager{
		Name: "my test azure vm mgr",
		Type: "azure-vm",
		CredentialData: []*common.Kv{
			{Key: "AZURE_CLIENT_ID", Value: os.Getenv("AZURE_CLIENT_ID")},
			{Key: "AZURE_CLIENT_SECRET", Value: os.Getenv("AZURE_CLIENT_SECRET")},
			{Key: "AZURE_TENANT_ID", Value: os.Getenv("AZURE_TENANT_ID")},
		},
	}
	mgrID, err := mgrClient.Create(ctx, &noCredsAzureMgr)
	require.NoError(t, err)
	require.NotZero(t, len(mgrID.Ids))

	// create a job with node manager reference
	filter := common.Filter{Key: "vj", Values: []string{"windows"}, Exclude: false}
	mgrFilter := jobs.ManagerFilter{
		ManagerId: mgrID.GetIds()[0].Id,
		Filters:   []*common.Filter{&filter},
	}
	job := jobs.Job{
		Name:          "my job for azure node manager",
		Tags:          []*common.Kv{},
		Type:          "exec",
		Profiles:      []string{"https://github.com/dev-sec/windows-baseline"},
		NodeSelectors: []*jobs.ManagerFilter{&mgrFilter},
	}
	t.Log("Creating job for node manager, to execute azure run command scan")
	jobID, err := jobsClient.Create(ctx, &job)
	require.NoError(t, err)

	// read the job to get the status
	jobRead, err := jobsClient.Read(ctx, jobID)
	require.NoError(t, err)
	status := jobRead.GetStatus()
	t.Log("Reading job status, looping until status reports as not 'new'.")
	for status == "new" {
		t.Logf("status: %s (sleeping 2s)", status)
		time.Sleep(2 * time.Second)
		jobRead, err := jobsClient.Read(ctx, jobID)
		require.NoError(t, err)
		status = jobRead.Status
	}
	require.NotEqual(t, "new", status)

	// since this is a remote execution job, we would need to have the a2 instance
	// be accessible from the instance being scanned if we wanted to have this work.
	// that is a thing we are choosing not to do for now, so we just check that the
	// job actually starts (by ensuring the status is not "new")
}
