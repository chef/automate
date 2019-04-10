package resolver

import (
	"testing"

	"github.com/chef/automate/api/external/secrets"
	"github.com/chef/automate/components/compliance-service/api/common"
	"github.com/chef/automate/components/compliance-service/inspec"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestGetNodeCredentials(t *testing.T) {
	secret := &secrets.Secret{
		Type: "ssh",
		Data: []*secrets.Kv{{Key: "username", Value: "my-user"}, {Key: "password", Value: "my-password"}},
	}
	inspecSecrets, err := getNodeCredentials(secret)
	require.NoError(t, err)
	assert.Equal(t, &inspec.Secrets{User: "my-user", Password: "my-password", KeyFiles: []string{""}}, inspecSecrets)

	secret = &secrets.Secret{
		Type: "winrm",
		Data: []*secrets.Kv{{Key: "username", Value: "my-user"}, {Key: "password", Value: "my-password"}},
	}
	inspecSecrets, err = getNodeCredentials(secret)
	require.NoError(t, err)
	assert.Equal(t, &inspec.Secrets{User: "my-user", Password: "my-password", KeyFiles: []string{""}}, inspecSecrets)

	secret = &secrets.Secret{
		Type: "sudo",
		Data: []*secrets.Kv{{Key: "password", Value: "my-sudo-password"}},
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
