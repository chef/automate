package integration

import (
	"context"
	"os"
	"time"

	"github.com/golang/protobuf/ptypes"

	"github.com/chef/automate/api/external/common/query"
	"github.com/chef/automate/api/external/compliance/reporting"
	"github.com/chef/automate/api/external/secrets"
	"github.com/chef/automate/api/interservice/compliance/common"
	"github.com/chef/automate/api/interservice/compliance/jobs"
	"github.com/chef/automate/api/interservice/nodemanager/manager"
	"github.com/chef/automate/api/interservice/nodemanager/nodes"
)

func (suite *GatewayTestSuite) TestLicenseUsageNodes() {
	suite.T().Skip("Skip LicenseUsageNodes test")
	// timestamp of now
	now := time.Now()
	startTime, err := ptypes.TimestampProto(now)
	suite.Require().NoError(err)

	// setup

	// get the gateway reporting client, since this is where the license
	// usage nodes api call exists
	reportingClient := reporting.NewReportingServiceClient(suite.gwConn)

	// setup secrets service for secret creation
	secretsClient, err := suite.clients.SecretClient()
	suite.Require().NoError(err)

	// setup compliance-service clients for job creation
	jobsClient, err := suite.clients.ComplianceJobsServiceClient()
	suite.Require().NoError(err)

	// setup nodemanager-service clients for node creation
	nodesClient, err := suite.clients.NodesClient()
	suite.Require().NoError(err)

	mgrClient, err := suite.clients.NodeManagerClient()
	suite.Require().NoError(err)

	// create secret for node
	secretID, err := secretsClient.Create(suite.ctx, &secrets.Secret{
		Name: "test secret",
		Type: "ssh",
		Data: []*query.Kv{
			{Key: "username", Value: suite.target.User},
			{Key: "key", Value: suite.target.Key},
		},
	})
	suite.Require().NoError(err)

	// create node
	nodeID, err := nodesClient.Create(suite.ctx, &nodes.Node{
		Name: "test node",
		TargetConfig: &nodes.TargetConfig{
			Backend: "ssh",
			Host:    suite.target.Host2,
			Port:    22,
			Secrets: []string{secretID.GetId()},
		},
	})
	suite.Require().NoError(err)

	// create manual node scan job
	_, err = jobsClient.Create(suite.ctx, &jobs.Job{
		Name:     "test job with manual node",
		Nodes:    []string{nodeID.GetId()},
		Type:     "exec",
		Profiles: []string{"https://github.com/dev-sec/ssl-baseline/archive/master.tar.gz"},
	})
	suite.Require().NoError(err)

	// create manual node scan job with recurrence of 2 times, every one minute
	_, err = jobsClient.Create(suite.ctx, &jobs.Job{
		Name:       "test job with manual node (recurring)",
		Nodes:      []string{nodeID.GetId()},
		Type:       "exec",
		Recurrence: "FREQ=MINUTELY;INTERVAL=1;COUNT=2",
		Profiles:   []string{"https://github.com/dev-sec/ssl-baseline/archive/master.tar.gz"},
	})
	suite.Require().NoError(err)

	// create aws-api scan job
	suite.T().Log("Creating aws-api node manager.")
	awsMgr := manager.NodeManager{
		Name: "my test aws api mgr",
		Type: "aws-api",
		CredentialData: []*common.Kv{
			{Key: "AWS_ACCESS_KEY_ID", Value: os.Getenv("AWS_ACCESS_KEY_ID")},
			{Key: "AWS_SECRET_ACCESS_KEY", Value: os.Getenv("AWS_SECRET_ACCESS_KEY")},
			{Key: "AWS_SESSION_TOKEN", Value: os.Getenv("AWS_SESSION_TOKEN")},
		},
	}

	mgrID, err := mgrClient.Create(context.Background(), &awsMgr)
	suite.Require().NoError(err)

	filter := common.Filter{Key: "region", Values: []string{"eu-west-1"}, Exclude: false}
	mgrFilter := jobs.ManagerFilter{
		ManagerId: mgrID.GetIds()[0].Id,
		Filters:   []*common.Filter{&filter},
	}
	job := jobs.Job{
		Name:          "my job for aws-api node manager",
		Tags:          []*common.Kv{},
		Type:          "exec",
		Profiles:      []string{"https://github.com/chef/automate/raw/0d2fc575a63fbd3d191834906ffb1e9d3a08f615/components/compliance-service/test_data/inspec_profiles/test-aws-profile-2.0.0.tar.gz"},
		NodeSelectors: []*jobs.ManagerFilter{&mgrFilter},
	}
	suite.T().Log("Creating job for aws-api node manager, to execute scan job")
	_, err = jobsClient.Create(context.Background(), &job)
	suite.Require().NoError(err)

	sleepCounter := 0
	// test results of license usage nodes api
	licenseNodes, err := reportingClient.LicenseUsageNodes(suite.ctx, &reporting.TimeQuery{StartTime: startTime})
	suite.Require().NoError(err)
	for licenseNodes.GetTotal() < 3 {
		suite.T().Logf("querying reporting nodes. counter at %d", sleepCounter)
		if sleepCounter == 30 {
			suite.T().Log("test timed out, reached 30 tries (10 min)")
			break
		}
		time.Sleep(time.Second * 20)
		// increase sleep counter and try again
		sleepCounter++
		licenseNodes, err = reportingClient.LicenseUsageNodes(suite.ctx, &reporting.TimeQuery{StartTime: startTime})
		suite.Require().NoError(err)
	}

	suite.T().Log("license nodes found: ", licenseNodes)
	suite.Equal(int32(3), licenseNodes.GetTotal())
	for _, report := range licenseNodes.GetReports() {
		// ensure none of the reports were 'aws-api' nodes
		suite.NotEqual("aws-api", report.GetEnvironment())
	}
}
