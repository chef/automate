package managers

import (
	"testing"

	"github.com/chef/automate/api/external/common/query"
	"github.com/chef/automate/api/external/secrets"
	"github.com/chef/automate/components/nodemanager-service/managers/awsec2"
	"github.com/stretchr/testify/assert"
)

func TestGetAWSCredsSetsDefaultRegionWhenNone(t *testing.T) {
	secret := &secrets.Secret{
		Data: []*query.Kv{
			{Key: "AWS_ACCESS_KEY_ID", Value: "fake-1"},
			{Key: "AWS_SECRET_ACCESS_KEY", Value: "fake-2"},
		},
	}
	creds := GetAWSCreds(secret)
	assert.Equal(t, awsec2.AwsCreds{
		AccessKeyId:     "fake-1",
		SecretAccessKey: "fake-2",
		Region:          "us-east-1",
	}, creds)
}

func TestGetAWSCredsSetsRegionWhenProvided(t *testing.T) {
	secret := &secrets.Secret{
		Data: []*query.Kv{
			{Key: "AWS_ACCESS_KEY_ID", Value: "fake-1"},
			{Key: "AWS_SECRET_ACCESS_KEY", Value: "fake-2"},
			{Key: "AWS_REGION", Value: "user-provided-region"},
		},
	}
	creds := GetAWSCreds(secret)
	assert.Equal(t, awsec2.AwsCreds{
		AccessKeyId:     "fake-1",
		SecretAccessKey: "fake-2",
		Region:          "user-provided-region",
	}, creds)
}
