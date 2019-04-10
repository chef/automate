package integration_test

import (
	"context"
	"testing"

	"github.com/chef/automate/api/external/secrets"
	"github.com/stretchr/testify/assert"
)

func TestCreateSecretEmpty(t *testing.T) {
	ctx := context.Background()
	req := new(secrets.Secret)

	_, err := secretsServer.Create(ctx, req)

	assert.Error(t, err)
}

func TestCreateSecretSuccessful(t *testing.T) {
	ctx := context.Background()

	dataSecrets := []struct {
		sType string
		data  []*secrets.Kv
	}{
		{
			sType: "ssh",
			data: appendKvs(&secrets.Kv{Key: "username", Value: "username"},
				&secrets.Kv{Key: "password", Value: "password"}),
		},
		{
			sType: "ssh",
			data: appendKvs(&secrets.Kv{Key: "username", Value: "username"},
				&secrets.Kv{Key: "key", Value: "key"}),
		},
		{
			sType: "service_now",
			data: appendKvs(&secrets.Kv{Key: "username", Value: "username"},
				&secrets.Kv{Key: "password", Value: "password"}),
		},
		{
			sType: "sudo",
			data:  appendKvs(&secrets.Kv{Key: "password", Value: "password"}),
		},
		{
			sType: "sudo",
			data:  appendKvs(&secrets.Kv{Key: "options", Value: "options"}),
		},
		{
			sType: "aws",
			data:  appendKvs(&secrets.Kv{Key: "ARN_ROLE", Value: "ARN_ROLE"}),
		},
		{
			sType: "aws",
			data: appendKvs(
				&secrets.Kv{Key: "ARN_ROLE", Value: ""},
				&secrets.Kv{Key: "AWS_ACCESS_KEY_ID", Value: "AWS_ACCESS_KEY_ID"},
				&secrets.Kv{Key: "AWS_SECRET_ACCESS_KEY", Value: "AWS_SECRET_ACCESS_KEY"}),
		},
		{
			sType: "azure",
			data: appendKvs(
				&secrets.Kv{Key: "AZURE_CLIENT_ID", Value: "AZURE_CLIENT_ID"},
				&secrets.Kv{Key: "AZURE_CLIENT_SECRET", Value: "AZURE_CLIENT_SECRET"},
				&secrets.Kv{Key: "AZURE_TENANT_ID", Value: "AZURE_TENANT_ID"}),
		},
		{
			// should this one fail, because we are not use a known type
			sType: "not_handled",
			data: appendKvs(&secrets.Kv{Key: "username", Value: "username"},
				&secrets.Kv{Key: "password", Value: "password"}),
		},
		{
			// should this one fail, because we are not storing any data
			sType: "not_handled",
		},
		{
			// should a type be required?
			data: appendKvs(&secrets.Kv{Key: "username", Value: "username"},
				&secrets.Kv{Key: "password", Value: "password"}),
		},
		{
			// should this one fail, because we are not storing any data and their is not type

		},
	}

	for _, data := range dataSecrets {
		initialSecret := &secrets.Secret{
			Name: "name",
			Type: data.sType,
			Data: data.data,
		}
		id, err := secretsServer.Create(ctx, initialSecret)
		assert.NoError(t, err)
		assert.NotNil(t, id)

		responseSecret, err := secretsServer.Read(ctx, &secrets.Id{Id: id.Id})
		assert.NoError(t, err)

		assert.Equal(t, id.Id, responseSecret.Id)
		assert.Equal(t, initialSecret.Name, responseSecret.Name)
		assert.Equal(t, initialSecret.Type, responseSecret.Type)
		assert.ElementsMatch(t, initialSecret.Data, responseSecret.Data)

		deleteAllSecrets()
	}
}

func TestCreateSecretFailure(t *testing.T) {
	ctx := context.Background()

	dataSecrets := []struct {
		sType string
		data  []*secrets.Kv
	}{
		{
			sType: "ssh",
		},
		{
			sType: "ssh",
			data:  appendKvs(&secrets.Kv{Key: "password", Value: "password"}),
		},
		{
			sType: "ssh",
			data:  appendKvs(&secrets.Kv{Key: "username", Value: "username"}),
		},
		{
			sType: "winrm",
		},
		{
			sType: "winrm",
			data:  appendKvs(&secrets.Kv{Key: "password", Value: "password"}),
		},
		{
			sType: "winrm",
			data:  appendKvs(&secrets.Kv{Key: "username", Value: "username"}),
		},
		{
			sType: "service_now",
		},
		{
			sType: "service_now",
			data:  appendKvs(&secrets.Kv{Key: "password", Value: "password"}),
		},
		{
			sType: "service_now",
			data:  appendKvs(&secrets.Kv{Key: "username", Value: "username"}),
		},
		{
			sType: "service_now",
			data:  appendKvs(&secrets.Kv{Key: "username", Value: "username"}, &secrets.Kv{Key: "key", Value: "key"}),
		},
		{
			sType: "sudo",
		},
		{
			sType: "aws",
		},
		{
			sType: "aws",
			data:  appendKvs(&secrets.Kv{Key: "ARN_ROLE", Value: ""}),
		},
		{
			sType: "aws",
			data: appendKvs(
				&secrets.Kv{Key: "ARN_ROLE", Value: ""},
				&secrets.Kv{Key: "AWS_SECRET_ACCESS_KEY", Value: "AWS_SECRET_ACCESS_KEY"}),
		},
		{
			sType: "aws",
			data: appendKvs(
				&secrets.Kv{Key: "ARN_ROLE", Value: ""},
				&secrets.Kv{Key: "AWS_ACCESS_KEY_ID", Value: "AWS_ACCESS_KEY_ID"}),
		},
		{
			sType: "azure",
		},
		{
			sType: "azure",
			data: appendKvs(
				&secrets.Kv{Key: "AZURE_CLIENT_SECRET", Value: "AZURE_CLIENT_SECRET"},
				&secrets.Kv{Key: "AZURE_TENANT_ID", Value: "AZURE_TENANT_ID"}),
		},
		{
			sType: "azure",
			data: appendKvs(
				&secrets.Kv{Key: "AZURE_CLIENT_ID", Value: "AZURE_CLIENT_ID"},
				&secrets.Kv{Key: "AZURE_TENANT_ID", Value: "AZURE_TENANT_ID"}),
		},
		{
			sType: "azure",
			data: appendKvs(
				&secrets.Kv{Key: "AZURE_CLIENT_ID", Value: "AZURE_CLIENT_ID"},
				&secrets.Kv{Key: "AZURE_CLIENT_SECRET", Value: "AZURE_CLIENT_SECRET"}),
		},
	}

	for _, data := range dataSecrets {
		initialSecret := &secrets.Secret{
			Name: "name",
			Type: data.sType,
			Data: data.data,
		}

		_, err := secretsServer.Create(ctx, initialSecret)

		assert.Error(t, err, "type: %s", data.sType)
	}
}
