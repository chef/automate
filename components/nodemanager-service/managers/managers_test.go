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

//AZURE
func TestAzureCredsWithOutSubscriptionID(t *testing.T) {
	secret := &secrets.Secret{
		Data: []*query.Kv{
			{Key: "AZURE_CLIENT_ID", Value: "fake-1"},
			{Key: "AZURE_CLIENT_SECRET", Value: "fake-2"},
			{Key: "AZURE_TENANT_ID", Value: "fake-3"},
		},
	}
	clientID, clientSecret, tenantID, subscriptionID := GetAzureCreds(secret)
	assert.Equal(t, "fake-1", clientID)
	assert.Equal(t, "fake-2", clientSecret)
	assert.Equal(t, "fake-3", tenantID)
	assert.Equal(t, "", subscriptionID)
}

func TestAzureCredsWithSubscriptionID(t *testing.T) {
	secret := &secrets.Secret{
		Data: []*query.Kv{
			{Key: "AZURE_CLIENT_ID", Value: "fake-1"},
			{Key: "AZURE_CLIENT_SECRET", Value: "fake-2"},
			{Key: "AZURE_TENANT_ID", Value: "fake-3"},
			{Key: "AZURE_SUBSCRIPTION_ID", Value: "fake-4"},
		},
	}
	clientID, clientSecret, tenantID, subscriptionID := GetAzureCreds(secret)
	assert.Equal(t, "fake-1", clientID)
	assert.Equal(t, "fake-2", clientSecret)
	assert.Equal(t, "fake-3", tenantID)
	assert.Equal(t, "fake-4", subscriptionID)
}
