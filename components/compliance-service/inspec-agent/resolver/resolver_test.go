package resolver

import (
	"testing"

	"github.com/chef/automate/api/external/common/query"
	"github.com/chef/automate/api/external/secrets"
	"github.com/chef/automate/api/interservice/compliance/common"
	"github.com/chef/automate/api/interservice/compliance/jobs"
	"github.com/chef/automate/api/interservice/nodemanager/manager"
	"github.com/chef/automate/api/interservice/nodemanager/nodes"
	"github.com/chef/automate/components/compliance-service/inspec"
	"github.com/chef/automate/components/nodemanager-service/managers/awsec2"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestAssembleAwsApiNodeInfo(t *testing.T) {
	node := &nodes.Node{
		Id:   "123",
		Name: "test-node-name",
	}
	m := &manager.NodeManager{
		Id:        "12300",
		Name:      "test-name",
		AccountId: "1234",
	}
	awsCreds := awsec2.AwsCreds{
		AccessKeyId:     "456",
		SecretAccessKey: "789",
		Region:          "test-region",
	}
	nodeDetails, tc, secrets := assembleAwsApiNodeInfo(node, m, awsCreds)

	assert.Equal(t, nodeInfo{
		UUID:           "123",
		CloudID:        "1234",
		Name:           "test-node-name",
		Environment:    "aws-api",
		ManagerID:      "12300",
		CloudAccountID: "1234",
	}, nodeDetails)

	assert.Equal(t, inspec.TargetBaseConfig{
		Backend: "aws",
		Region:  "test-region",
	}, tc)

	assert.Equal(t, inspec.Secrets{
		AwsUser:     "456",
		AwsPassword: "789",
	}, secrets)

	// test the case when no creds were provided
	awsCreds = awsec2.AwsCreds{}
	nodeDetails, tc, secrets = assembleAwsApiNodeInfo(node, m, awsCreds)
	assert.Equal(t, inspec.TargetBaseConfig{
		Backend: "aws",
		Region:  "us-east-1",
	}, tc)

	node.TargetConfig = &nodes.TargetConfig{
		Region: "node-set-region",
	}
	nodeDetails, tc, secrets = assembleAwsApiNodeInfo(node, m, awsCreds)
	assert.Equal(t, inspec.TargetBaseConfig{
		Backend: "aws",
		Region:  "node-set-region",
	}, tc)

}
func TestHandleSSMNodes(t *testing.T) {
	ssmJob, skip := false, false
	node := &manager.ManagerNode{
		Ssm: "Online",
	}
	job := &jobs.Job{
		Type: "detect",
	}
	backend := "ssh"

	ssmJob, skip = handleSSMNodes(node, job, &backend)
	assert.Equal(t, true, ssmJob)
	assert.Equal(t, true, skip)
	assert.Equal(t, inspec.BackendSSM, backend)

	backend = "ssh"
	node.Ssm = "Online:Azure"
	ssmJob, skip = handleSSMNodes(node, job, &backend)
	assert.Equal(t, true, ssmJob)
	assert.Equal(t, true, skip)
	assert.Equal(t, inspec.BackendAZ, backend)

	backend = "ssh"
	job.Type = "exec"
	node.Ssm = "Online"
	ssmJob, skip = handleSSMNodes(node, job, &backend)
	assert.Equal(t, true, ssmJob)
	assert.Equal(t, false, skip)
	assert.Equal(t, inspec.BackendSSM, backend)

	backend = "ssh"
	node.Ssm = ""
	ssmJob, skip = handleSSMNodes(node, job, &backend)
	assert.Equal(t, false, ssmJob)
	assert.Equal(t, false, skip)
	assert.Equal(t, "ssh", backend)

	backend = "ssh"
	job.Type = "exec"
	ssmJob, skip = handleSSMNodes(node, job, &backend)
	assert.Equal(t, false, ssmJob)
	assert.Equal(t, false, skip)
	assert.Equal(t, "ssh", backend)

}

func TestGetNodeCredentials(t *testing.T) {
	secret := &secrets.Secret{
		Type: "ssh",
		Data: []*query.Kv{{Key: "username", Value: "my-user"}, {Key: "password", Value: "my-password"}},
	}
	inspecSecrets, err := getNodeCredentials(secret)
	require.NoError(t, err)
	assert.Equal(t, &inspec.Secrets{User: "my-user", Password: "my-password", KeyFiles: []string{""}}, inspecSecrets)

	secret = &secrets.Secret{
		Type: "winrm",
		Data: []*query.Kv{{Key: "username", Value: "my-user"}, {Key: "password", Value: "my-password"}},
	}
	inspecSecrets, err = getNodeCredentials(secret)
	require.NoError(t, err)
	assert.Equal(t, &inspec.Secrets{User: "my-user", Password: "my-password", KeyFiles: []string{""}}, inspecSecrets)

	secret = &secrets.Secret{
		Type: "sudo",
		Data: []*query.Kv{{Key: "password", Value: "my-sudo-password"}},
	}
	inspecSecrets, err = getNodeCredentials(secret)
	require.NoError(t, err)
	assert.Equal(t, &inspec.Secrets{SudoPassword: "my-sudo-password", KeyFiles: []string{""}}, inspecSecrets)
}

func TestHandleRegionFilters(t *testing.T) {
	regions := []string{"ap-south-1", "eu-west-3", "eu-west-2", "eu-west-1", "ap-northeast-2", "ap-northeast-1", "sa-east-1", "ca-central-1", "ap-southeast-1", "ap-southeast-2", "eu-central-1", "us-east-1", "us-east-2", "us-west-1", "us-west-2"}

	filters := []*common.Filter{
		{Key: "region", Values: []string{"eu-west*"}},
		{Key: "region", Values: []string{"eu-w*"}},
		{Key: "region", Values: []string{"us-west-1"}},
	}
	expected := []string{"eu-west-1", "eu-west-2", "eu-west-3", "us-west-1"}
	incRegions, err := handleRegionFilters(filters, regions)
	assert.NoError(t, err)
	assert.ElementsMatch(t, expected, incRegions)

	filters = []*common.Filter{}
	expected = regions
	incRegions, err = handleRegionFilters(filters, regions)
	assert.NoError(t, err)
	assert.ElementsMatch(t, expected, incRegions)

	filters = []*common.Filter{
		{Key: "region", Values: []string{"ap-*"}, Exclude: true},
		{Key: "region", Values: []string{"eu-*", "sa-*", "ca-central-1"}, Exclude: true},
	}
	expected = []string{"us-east-1", "us-east-2", "us-west-1", "us-west-2"}
	incRegions, err = handleRegionFilters(filters, regions)
	assert.NoError(t, err)
	assert.ElementsMatch(t, expected, incRegions)
}

func TestAssembleJob(t *testing.T) {
	t.Log("test assembleJob when no node id included generates a node uuid and assigns it to the node")
	job, err := assembleJob(&jobs.Job{}, nodeInfo{Name: "test"}, []*inspec.Secrets{}, inspec.TargetBaseConfig{})
	assert.NoError(t, err)

	assert.NotZero(t, job.NodeID)

	t.Log("test assembleJob when only one secret is included assigns it to secrets")
	job, err = assembleJob(&jobs.Job{}, nodeInfo{Name: "test"}, []*inspec.Secrets{
		{User: "test", Password: "test"},
	}, inspec.TargetBaseConfig{})
	assert.NoError(t, err)
	assert.NotZero(t, job.TargetConfig.Secrets)
	assert.Zero(t, job.TargetConfig.SecretsArr)

	t.Log("test assembleJob when more than one secret is included assigns it to secretsArr")
	job, err = assembleJob(&jobs.Job{}, nodeInfo{Name: "test"}, []*inspec.Secrets{
		{User: "test", Password: "test"},
		{User: "test2", Password: "test2"},
	}, inspec.TargetBaseConfig{})
	assert.NoError(t, err)
	assert.Zero(t, job.TargetConfig.Secrets)
	assert.NotZero(t, job.TargetConfig.SecretsArr)
}

func TestnodeInfoFromManagerNodeCorrectlyAssignsName(t *testing.T) {
	nodeDetails := nodeInfoFromManagerNode(&manager.ManagerNode{
		Name: "test-name",
		Host: "localhost",
	})
	assert.Equal(t, "test-name", nodeDetails.Name)

	nodeDetails = nodeInfoFromManagerNode(&manager.ManagerNode{
		Name: "",
		Host: "localhost",
	})
	assert.Equal(t, "localhost", nodeDetails.Name)
}

func TestnodeInfoFromManagerNodeObservesTagValues(t *testing.T) {
	nodeDetails := nodeInfoFromManagerNode(&manager.ManagerNode{
		Name: "test-name",
		Host: "localhost",
		Tags: []*common.Kv{
			{Key: "Name", Value: "tag-named-instance"},
		},
	})
	assert.Equal(t, "tag-named-instance", nodeDetails.Name)

	nodeDetails = nodeInfoFromManagerNode(&manager.ManagerNode{
		Name: "test-name",
		Host: "localhost",
		Tags: []*common.Kv{
			{Key: "Environment", Value: "test-env"},
		},
	})
	assert.Equal(t, "test-env", nodeDetails.Environment)
}
