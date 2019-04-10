package integration_test

import (
	"context"
	"testing"

	"github.com/chef/automate/api/external/secrets"
	"github.com/stretchr/testify/assert"
)

func TestReadSecretEmpty(t *testing.T) {
	ctx := context.Background()
	req := new(secrets.Id)

	_, err := secretsServer.Read(ctx, req)

	assert.Error(t, err)
}

func TestReadSecretInvalidID(t *testing.T) {
	ctx := context.Background()
	req := &secrets.Id{Id: "Invalid"}

	_, err := secretsServer.Read(ctx, req)

	assert.Error(t, err)
}

func TestReadSecretCreateAndRead(t *testing.T) {
	ctx := context.Background()
	initialSecret := &secrets.Secret{
		Name: "name",
		Type: "ssh",
		Data: appendKvs(
			&secrets.Kv{Key: "username", Value: "username"},
			&secrets.Kv{Key: "password", Value: "password"}),
	}

	// Create one secret
	id, err := secretsServer.Create(ctx, initialSecret)
	assert.NoError(t, err)
	assert.NotNil(t, id)

	responseSecret, err := secretsServer.Read(ctx, &secrets.Id{Id: id.Id})
	assert.NoError(t, err)
	assert.NotNil(t, responseSecret)

	assert.Equal(t, id.Id, responseSecret.Id)
	assert.Equal(t, initialSecret.Name, responseSecret.Name)
	assert.Equal(t, initialSecret.Type, responseSecret.Type)
	assert.ElementsMatch(t, initialSecret.Data, responseSecret.Data)
}
