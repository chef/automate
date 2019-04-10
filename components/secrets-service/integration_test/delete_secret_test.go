package integration_test

import (
	"context"
	"testing"

	"github.com/chef/automate/api/external/secrets"
	"github.com/stretchr/testify/assert"
)

func TestDeleteSecretEmpty(t *testing.T) {
	ctx := context.Background()
	req := new(secrets.Id)

	_, err := secretsServer.Delete(ctx, req)

	assert.NoError(t, err)
}

func TestDeleteSecretInvalidID(t *testing.T) {
	ctx := context.Background()
	req := &secrets.Id{Id: "Invalid"}

	_, err := secretsServer.Delete(ctx, req)

	assert.NoError(t, err)
}

func TestDeleteSecretCreateAndDelete(t *testing.T) {
	ctx := context.Background()
	req := &secrets.Secret{
		Name: "name",
		Type: "ssh",
		Data: appendKvs(
			&secrets.Kv{Key: "username", Value: "username"},
			&secrets.Kv{Key: "password", Value: "password"}),
	}

	// Create one secret
	id, err := secretsServer.Create(ctx, req)
	assert.NoError(t, err)
	assert.NotNil(t, id)

	// Check that there is one secret
	list, err := secretsServer.List(ctx, &secrets.Query{})
	assert.NoError(t, err)
	assert.Equal(t, int32(1), list.GetTotal())

	_, err = secretsServer.Delete(ctx, &secrets.Id{Id: id.Id})
	assert.NoError(t, err)

	// Check that there are zero secrets
	list, err = secretsServer.List(ctx, &secrets.Query{})
	assert.NoError(t, err)
	assert.Equal(t, int32(0), list.GetTotal())
}
