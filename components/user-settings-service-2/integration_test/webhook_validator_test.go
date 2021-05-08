package integration_test

import (
	"testing"

	"github.com/chef/automate/api/external/common/query"
	"github.com/chef/automate/api/external/secrets"
	api "github.com/chef/automate/api/interservice/notifications/service"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestWebhookValidator(t *testing.T) {
	t.Run("with a malformed URL, the webhook validator returns an 'invalid URL' response", func(t *testing.T) {
		res, err := suite.Client.ValidateWebhook(ctx, &api.URLValidationRequest{
			Url:         "malformed_URL_input",
			Credentials: &api.URLValidationRequest_None{},
		})
		require.NoError(t, err)
		assert.Equal(t, api.URLValidationResponse_INVALID_URL, res.Code)
	})

	t.Run("with a URL for a service that isn't running, the webhook validator returns an error", func(t *testing.T) {
		ts := newTestServer()
		// Note this is closed immediately instead of defered, so the server will be down.
		ts.Close()

		res, err := suite.Client.ValidateWebhook(ctx, &api.URLValidationRequest{
			Url:         ts.SlackURL(),
			Credentials: &api.URLValidationRequest_None{},
		})
		require.NoError(t, err)
		assert.Equal(t, api.URLValidationResponse_ERROR, res.Code)
	})

	t.Run("with a valid URL and no credentials, the webhook validator succeeds", func(t *testing.T) {
		ts := newTestServer()
		defer ts.Close()

		res, err := suite.Client.ValidateWebhook(ctx, &api.URLValidationRequest{
			Url:         ts.SlackURL(),
			Credentials: &api.URLValidationRequest_None{},
		})
		require.NoError(t, err)
		assert.Equal(t, api.URLValidationResponse_OK, res.Code)

		postData, err := ts.GetLastPost()
		require.NoError(t, err)
		assert.NotNil(t, postData)
	})

	t.Run("with a valid URL and username/password creds, the webhook validator succeeds", func(t *testing.T) {
		ts := newTestServer()
		defer ts.Close()

		res, err := suite.Client.ValidateWebhook(ctx, &api.URLValidationRequest{
			Url: ts.SlackURL(),
			Credentials: &api.URLValidationRequest_UsernamePassword{
				UsernamePassword: &api.UsernamePassword{
					Username: "integration_test_username",
					Password: "integration_test_password",
				},
			},
		})
		require.NoError(t, err)
		assert.Equal(t, api.URLValidationResponse_OK, res.Code)

		postData, err := ts.GetLastPost()
		require.NoError(t, err)
		assert.NotNil(t, postData)

		assert.True(t, postData.BasicAuthUsed)
		assert.Equal(t, "integration_test_username", postData.BasicAuthUsername)
		assert.Equal(t, "integration_test_password", postData.BasicAuthPassword)
	})

	t.Run("with a valid URL and secret ID creds, and the secret ID **IS NOT** in the secrets store, the webhook validator succeeds", func(t *testing.T) {
		ts := newTestServer()
		defer ts.Close()

		res, err := suite.Client.ValidateWebhook(ctx, &api.URLValidationRequest{
			Url: ts.SlackURL(),
			Credentials: &api.URLValidationRequest_SecretId{
				SecretId: &api.SecretId{
					Id: "integration_test_secret_id_NOEXIST",
				},
			},
		})
		require.NoError(t, err)
		assert.Equal(t, api.URLValidationResponse_OK, res.Code)

		postData, err := ts.GetLastPost()
		require.NoError(t, err)
		assert.NotNil(t, postData)
		assert.False(t, postData.BasicAuthUsed)
	})

	t.Run("with a valid URL and secret ID creds, and the secret ID **IS** in the secrets store, the webhook validator succeeds", func(t *testing.T) {
		ts := newTestServer()
		defer ts.Close()

		sres, err := suite.SecretsClient.Create(ctx, &secrets.Secret{
			Id:   "integration_test_secret_id",
			Name: "integration_test_secret_id",
			Type: "service_now",
			Data: []*query.Kv{
				&query.Kv{Key: "username", Value: "integration_test_username_secretstore"},
				&query.Kv{Key: "password", Value: "integration_test_password_secretstore"},
			},
		})
		require.NoError(t, err)
		require.NotEmpty(t, sres.Id)

		secretID := sres.Id
		defer func() {
			suite.SecretsClient.Delete(ctx, &secrets.Id{Id: secretID})
		}()

		res, err := suite.Client.ValidateWebhook(ctx, &api.URLValidationRequest{
			Url: ts.SlackURL(),
			Credentials: &api.URLValidationRequest_SecretId{
				SecretId: &api.SecretId{
					Id: secretID,
				},
			},
		})
		require.NoError(t, err)
		assert.Equal(t, api.URLValidationResponse_OK, res.Code)

		postData, err := ts.GetLastPost()
		require.NoError(t, err)
		assert.NotNil(t, postData)
		assert.True(t, postData.BasicAuthUsed)
		assert.Equal(t, "integration_test_username_secretstore", postData.BasicAuthUsername)
		assert.Equal(t, "integration_test_password_secretstore", postData.BasicAuthPassword)
	})
}
