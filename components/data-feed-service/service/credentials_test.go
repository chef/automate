package service

import (
	"context"
	"reflect"
	"testing"

	"github.com/chef/automate/api/external/common/query"

	"github.com/chef/automate/api/external/secrets"
	"github.com/golang/mock/gomock"
)

const username = "data"
const password = "feed"
const basicAuthHeaderValue = "Basic ZGF0YTpmZWVk"
const splunkToken = "s45197c0-01b9-4184-a2c5-2d6ed45095cdf"
const splunkHeaderValue = "Splunk " + splunkToken
const secretId = "secret-id"

func TestNewCredentialsFactoryNilData(t *testing.T) {
	credentialsFactory := NewCredentialsFactory(nil)
	credentials, err := credentialsFactory.NewCredentials()
	if err.Error() != CredentialsError {
		t.Logf("expected error: %v, got %v", CredentialsError, err)
		t.Fail()
	}
	if credentials != nil {
		t.Logf("expected credentials: nil, got %v", credentials)
		t.Fail()
	}
}

func TestNewCredentialsFactoryUnsupportedData(t *testing.T) {
	data := map[string]string{"unsupported key": "unsupported value"}
	credentialsFactory := NewCredentialsFactory(data)
	credentials, err := credentialsFactory.NewCredentials()
	if err.Error() != CredentialsError {
		t.Logf("expected error: %v, got %v", CredentialsError, err)
		t.Fail()
	}
	if credentials != nil {
		t.Logf("expected credentials: nil, got %v", credentials)
		t.Fail()
	}
}

func TestNewBasicAuthCredentials(t *testing.T) {

	credentials := NewBasicAuthCredentials(username, password)
	if credentials.username != username {
		t.Logf("expected username: %s, got: %s", username, credentials.username)
		t.Fail()
	}
	if credentials.password != password {
		t.Logf("expected password: %s, got: %s", password, credentials.password)
		t.Fail()
	}
}

func TestBasicAuthCredentials(t *testing.T) {
	credentials := &BasicAuthCredentials{username: username, password: password}
	if credentials.username != username {
		t.Logf("expected username: %s, got: %s", username, credentials.username)
		t.Fail()
	}
	if credentials.password != password {
		t.Logf("expected password: %s, got: %s", password, credentials.password)
		t.Fail()
	}
}

func TestNewCredentialsFactoryBasicAuth(t *testing.T) {
	data := map[string]string{"username": username, "password": password}
	credentialsFactory := NewCredentialsFactory(data)
	credentials, err := credentialsFactory.NewCredentials()
	if err != nil {
		t.Logf("expected error: nil, got %v", err)
		t.Fail()
	}
	if credentials == nil {
		t.Logf("expected credentials, got nil")
		t.Fail()
	}
	switch credentials.(type) {
	case BasicAuthCredentials:
		break
	default:
		t.Logf("Expected BasicAuthCredentials, got %v", reflect.TypeOf(credentials))
		t.Fail()
	}
	if credentials.GetAuthorizationHeaderValue() != basicAuthHeaderValue {
		t.Logf("Expected header %v, got %v", basicAuthHeaderValue, credentials.GetAuthorizationHeaderValue())
		t.Fail()
	}
}

func TestNewSplunkAuthCredentials(t *testing.T) {

	credentials := NewSplunkAuthCredentials(splunkToken)
	if credentials.splunkToken != splunkToken {
		t.Logf("expected splunkToken: %s, got: %s", splunkToken, credentials.splunkToken)
		t.Fail()
	}
}

func TestSplunkAuthCredentials(t *testing.T) {

	credentials := &SplunkAuthCredentials{splunkToken: splunkToken}
	if credentials.splunkToken != splunkToken {
		t.Logf("expected splunkToken: %s, got: %s", splunkToken, credentials.splunkToken)
		t.Fail()
	}
}

func TestNewCredentialsFactorySplunk(t *testing.T) {
	data := map[string]string{"Splunk": splunkToken}
	credentialsFactory := NewCredentialsFactory(data)
	credentials, err := credentialsFactory.NewCredentials()
	if err != nil {
		t.Logf("expected error: nil, got %v", err)
		t.Fail()
	}
	if credentials == nil {
		t.Logf("expected credentials, got nil")
		t.Fail()
	}
	switch credentials.(type) {
	case SplunkAuthCredentials:
		break
	default:
		t.Logf("Expected SplunkAuthCredentials, got %v", reflect.TypeOf(credentials))
		t.Fail()
	}
	if credentials.GetAuthorizationHeaderValue() != splunkHeaderValue {
		t.Logf("Expected header %v, got %v", splunkHeaderValue, credentials.GetAuthorizationHeaderValue())
		t.Fail()
	}
}

func TestGetCredentialsError(t *testing.T) {
	secretsClient := secrets.NewMockSecretsServiceClient(gomock.NewController(t))
	secretsClient.EXPECT().Read(
		context.Background(),
		gomock.Any(),
	).Return(nil, mockErr)
	credentials, err := GetCredentials(context.Background(), secretsClient, secretId)

	if err == nil {
		t.Logf("Expected error: %v, got %v", mockErr, err)
		t.Fail()
	}
	if credentials != nil {
		t.Logf("Expected credentials: nil, got %v", credentials)
		t.Fail()
	}
}

func TestGetCredentialsUnsupported(t *testing.T) {
	secret := &secrets.Secret{
		Name: "name",
		Type: "ssh",
		Data: appendKvs(&query.Kv{Key: "unsupported", Value: "value"}),
	}
	secretsClient := secrets.NewMockSecretsServiceClient(gomock.NewController(t))
	secretsClient.EXPECT().Read(
		context.Background(),
		gomock.Any(),
	).Return(secret, nil)

	credentials, err := GetCredentials(context.Background(), secretsClient, secretId)

	if err == nil {
		t.Logf("Expected error: %v, got %v", mockErr, err)
		t.Fail()
	}
	if credentials != nil {
		t.Logf("Expected credentials: nil, got %v", credentials)
		t.Fail()
	}
}

func TestGetCredentialsBasicAuth(t *testing.T) {
	secret := &secrets.Secret{
		Name: "name",
		Type: "ssh",
		Data: appendKvs(&query.Kv{Key: "username", Value: username}, &query.Kv{Key: "password", Value: password}),
	}
	secretsClient := secrets.NewMockSecretsServiceClient(gomock.NewController(t))
	secretsClient.EXPECT().Read(
		context.Background(),
		gomock.Any(),
	).Return(secret, nil)
	// need to Mock the secret using witout a splunk or basic auth
	credentials, err := GetCredentials(context.Background(), secretsClient, secretId)

	if err != nil {
		t.Logf("Expected error: nil, got %v", err)
		t.Fail()
	}
	if credentials == nil {
		t.Log("Expected credentials, got nil")
		t.Fail()
	} else if credentials.GetAuthorizationHeaderValue() != basicAuthHeaderValue {
		t.Logf("Expected header: %s, got %s", basicAuthHeaderValue, credentials.GetAuthorizationHeaderValue())
		t.Fail()
	}
}

func TestGetCredentialsSplunk(t *testing.T) {
	secret := &secrets.Secret{
		Name: "name",
		Type: "ssh",
		Data: appendKvs(&query.Kv{Key: "Splunk", Value: splunkToken}),
	}
	secretsClient := secrets.NewMockSecretsServiceClient(gomock.NewController(t))
	secretsClient.EXPECT().Read(
		context.Background(),
		gomock.Any(),
	).Return(secret, nil)

	credentials, err := GetCredentials(context.Background(), secretsClient, secretId)

	if err != nil {
		t.Logf("Expected error: nil, got %v", err)
		t.Fail()
	}
	if credentials == nil {
		t.Log("Expected credentials, got nil")
		t.Fail()
	} else if credentials.GetAuthorizationHeaderValue() != splunkHeaderValue {
		t.Logf("Expected header: %s, got %s", splunkHeaderValue, credentials.GetAuthorizationHeaderValue())
		t.Fail()
	}
}

func appendKvs(kvs ...*query.Kv) []*query.Kv {
	array := make([]*query.Kv, len(kvs))

	for i, kv := range kvs {
		array[i] = kv
	}

	return array
}
