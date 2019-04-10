package integration_test

import (
	"context"
	"testing"

	"github.com/chef/automate/api/external/secrets"
	"github.com/stretchr/testify/assert"
)

func TestUpdateSecretEmpty(t *testing.T) {
	ctx := context.Background()
	req := new(secrets.Secret)

	_, err := secretsServer.Update(ctx, req)

	assert.Error(t, err)
}

func TestUpdateSecretInvalidID(t *testing.T) {
	ctx := context.Background()
	req := &secrets.Secret{Id: "Invalid"}

	_, err := secretsServer.Update(ctx, req)

	assert.Error(t, err)
}

func TestReadSecretCreateAndUpdate(t *testing.T) {
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

	updateSecret := &secrets.Secret{
		Id:   id.Id,
		Name: "newname",
		Type: "ssh",
		Data: appendKvs(
			&secrets.Kv{Key: "username", Value: "new_username"},
			&secrets.Kv{Key: "password", Value: "new_password"}),
	}

	_, err = secretsServer.Update(ctx, updateSecret)
	assert.NoError(t, err)

	responseSecret2, err := secretsServer.Read(ctx, &secrets.Id{Id: id.Id})
	assert.NoError(t, err)
	assert.NotNil(t, responseSecret2)

	assert.Equal(t, id.Id, responseSecret2.Id)
	assert.Equal(t, updateSecret.Name, responseSecret2.Name)
	assert.Equal(t, updateSecret.Type, responseSecret2.Type)
	assert.ElementsMatch(t, updateSecret.Data, responseSecret2.Data)
}
