package compliance

import (
	"context"
	"os"
	"testing"
	"time"

	"github.com/golang/protobuf/ptypes"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	"github.com/chef/automate/api/external/secrets"
	"github.com/chef/automate/components/automate-gateway/api/compliance/reporting"
	"github.com/chef/automate/components/compliance-service/api/common"
	"github.com/chef/automate/components/compliance-service/api/jobs"
	"github.com/chef/automate/components/compliance-service/examples/helpers"
	"github.com/chef/automate/components/nodemanager-service/api/manager"
	"github.com/chef/automate/components/nodemanager-service/api/nodes"
)

const (
	complianceEndpoint = "127.0.0.1:10121"
	secretsEndpoint    = "127.0.0.1:10131"
	gatewayEndpoint    = "127.0.0.1:2001"
	nodesEndpoint      = "127.0.0.1:10120"
)

func TestLicenseUsageNodes(t *testing.T) {
	t.Skip("Skip LicenseUsageNodes test")
	// timestamp of now
	now := time.Now()
	startTime, err := ptypes.TimestampProto(now)
	require.NoError(t, err)

	// setup
	ctx := context.Background()
	connFactory := helpers.SecureConnFactoryHabWithDeploymentServiceCerts()

	// get the gateway reporting client, since this is where the license
	// usage nodes api call exists
	gatewayConn, err := connFactory.Dial("automate-gateway", gatewayEndpoint)
	require.NoError(t, err)
	defer gatewayConn.Close()
	reportingClient := reporting.NewReportingServiceClient(gatewayConn)

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
	mgrClient := manager.NewNodeManagerServiceClient(nodesConn)
	nodesClient := nodes.NewNodesServiceClient(nodesConn)

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

	// create node
	nodeId, err := nodesClient.Create(ctx, &nodes.Node{
		Name: "test node",
		TargetConfig: &nodes.TargetConfig{
			Backend: "ssh",
			Host:    os.Getenv("AUTOMATE_ACCEPTANCE_TARGET_HOST2"),
			Port:    22,
			Secrets: []string{secretId.GetId()},
		},
	})
	require.NoError(t, err)

	// create manual node scan job
	_, err = jobsClient.Create(ctx, &jobs.Job{
		Name:     "test job with manual node",
		Nodes:    []string{nodeId.GetId()},
		Type:     "exec",
		Profiles: []string{"https://github.com/dev-sec/apache-baseline/archive/master.tar.gz"},
	})
	require.NoError(t, err)

	// create manual node scan job with recurrence of 2 times, every one minute
	_, err = jobsClient.Create(ctx, &jobs.Job{
		Name:       "test job with manual node (recurring)",
		Nodes:      []string{nodeId.GetId()},
		Type:       "exec",
		Recurrence: "FREQ=MINUTELY;INTERVAL=1;COUNT=2",
		Profiles:   []string{"https://github.com/dev-sec/apache-baseline/archive/master.tar.gz"},
	})
	require.NoError(t, err)

	// create aws-api scan job
	t.Log("Creating aws-api node manager.")
	awsMgr := manager.NodeManager{
		Name: "my test aws api mgr",
		Type: "aws-api",
		CredentialData: []*common.Kv{
			{Key: "AWS_ACCESS_KEY_ID", Value: os.Getenv("AWS_ACCESS_KEY_ID")},
			{Key: "AWS_SECRET_ACCESS_KEY", Value: os.Getenv("AWS_SECRET_ACCESS_KEY")},
			{Key: "AWS_SESSION_TOKEN", Value: os.Getenv("AWS_SESSION_TOKEN")},
		},
	}

	mgrId, err := mgrClient.Create(context.Background(), &awsMgr)
	require.NoError(t, err)

	filter := common.Filter{Key: "region", Values: []string{"eu-west-1"}, Exclude: false}
	mgrFilter := jobs.ManagerFilter{
		ManagerId: mgrId.GetIds()[0].Id,
		Filters:   []*common.Filter{&filter},
	}
	job := jobs.Job{
		Name:          "my job for aws-api node manager",
		Tags:          []*common.Kv{},
		Type:          "exec",
		Profiles:      []string{"https://github.com/vjeffrey/try-inspec-aws-profile/archive/master.tar.gz"},
		NodeSelectors: []*jobs.ManagerFilter{&mgrFilter},
	}
	t.Log("Creating job for aws-api node manager, to execute scan job")
	_, err = jobsClient.Create(context.Background(), &job)
	require.NoError(t, err)

	sleepCounter := 0
	// test results of license usage nodes api
	licenseNodes, err := reportingClient.LicenseUsageNodes(ctx, &reporting.TimeQuery{StartTime: startTime})
	require.NoError(t, err)
	for licenseNodes.GetTotal() < 3 {
		t.Logf("querying reporting nodes. counter at %d", sleepCounter)
		if sleepCounter == 30 {
			t.Log("test timed out, reached 30 tries (10 min)")
			break
		}
		time.Sleep(time.Second * 20)
		// increase sleep counter and try again
		sleepCounter++
		licenseNodes, err = reportingClient.LicenseUsageNodes(ctx, &reporting.TimeQuery{StartTime: startTime})
		require.NoError(t, err)
	}

	t.Log("license nodes found: ", licenseNodes)
	assert.Equal(t, int32(3), licenseNodes.GetTotal())
	for _, report := range licenseNodes.GetReports() {
		// ensure none of the reports were 'aws-api' nodes
		assert.NotEqual(t, "aws-api", report.GetEnvironment())
	}
}
