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
const bearerToken = "Bearer t7KxtPZQQ5VOVBjKWtqS12DGpFHaDTuadJA"
const splunkHeaderValue = "Splunk " + splunkToken
const secretId = "secret-id"
const customHeaders = "{\"Authorization\":\"Basic 6f01b869-c181-4fb2-a74c-b619e6197a85\",\"x-chef-api\":\"endpoint\"}"
const accessKey = "QWERTYUIOPASDFGHJKLZ"
const secretAccessKey = "secretAccessKeyValue"
const bucket = "store.in.bucket"
const region = "us-east-2"
const serviceServiceNow = "Service Now"
const serviceS3 = "S3"
const serviceMinio = "Minio"
const serviceSplunk = "Splunk"

func TestNewCredentialsFactoryNilData(t *testing.T) {
	credentialsFactory := NewCredentialsFactory(nil)
	dbData := DBData{Services: serviceServiceNow, IntegrationType: Webhook, MetaData: "{}"}

	credentials, err := credentialsFactory.NewCredentials(dbData)
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
	dbData := DBData{Services: serviceServiceNow, IntegrationType: Webhook, MetaData: "{}"}

	credentials, err := credentialsFactory.NewCredentials(dbData)
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
	dbData := DBData{Services: "Service Now", IntegrationType: Webhook, MetaData: "{}"}

	credentials, err := credentialsFactory.NewCredentials(dbData)
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
	if credentials.GetValues().AuthorizationHeader != basicAuthHeaderValue {
		t.Logf("Expected header %v, got %v", basicAuthHeaderValue, credentials.GetValues().AuthorizationHeader)
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
	dbData := DBData{Services: serviceSplunk, IntegrationType: Webhook, MetaData: "MetaData"}

	credentials, err := credentialsFactory.NewCredentials(dbData)
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
	if credentials.GetValues().AuthorizationHeader != splunkHeaderValue {
		t.Logf("Expected header %v, got %v", splunkHeaderValue, credentials.GetValues().AuthorizationHeader)
		t.Fail()
	}
}

func TestGetCredentialsError(t *testing.T) {
	secretsClient := secrets.NewMockSecretsServiceClient(gomock.NewController(t))
	secretsClient.EXPECT().Read(
		context.Background(),
		gomock.Any(),
	).Return(nil, mockErr)
	credentials, err := GetCredentials(context.Background(), secretsClient, secretId, "Services", "IntegrationType", "{}")

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

	credentials, err := GetCredentials(context.Background(), secretsClient, secretId, "Services", "IntegrationType", "{}")

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
	credentials, err := GetCredentials(context.Background(), secretsClient, secretId, serviceServiceNow, Webhook, "{}")

	if err != nil {
		t.Logf("Expected error: nil, got %v", err)
		t.Fail()
	}
	if credentials == nil {
		t.Log("Expected credentials, got nil")
		t.Fail()
	} else if credentials.GetValues().AuthorizationHeader != basicAuthHeaderValue {
		t.Logf("Expected header: %s, got %s", basicAuthHeaderValue, credentials.GetValues().AuthorizationHeader)
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

	credentials, err := GetCredentials(context.Background(), secretsClient, secretId, serviceSplunk, Webhook, "{}")

	if err != nil {
		t.Logf("Expected error: nil, got %v", err)
		t.Fail()
	}
	if credentials == nil {
		t.Log("Expected credentials, got nil")
		t.Fail()
	} else if credentials.GetValues().AuthorizationHeader != splunkHeaderValue {
		t.Logf("Expected header: %s, got %s", splunkHeaderValue, credentials.GetValues().AuthorizationHeader)
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

func TestNewCustomHeadersAuthCredentials(t *testing.T) {

	credentials := NewCustomHeaderCredentials(customHeaders)
	if credentials.headers != customHeaders {
		t.Logf("expected token: %s, got: %s", customHeaders, credentials.headers)
		t.Fail()
	}
}

func TestCustomHeadersCredentials(t *testing.T) {

	credentials := &CustomHeaderCredentials{headers: customHeaders}
	if credentials.headers != customHeaders {
		t.Logf("expected token: %s, got: %s", customHeaders, credentials.headers)
		t.Fail()
	}
}

func TestNewCredentialsFactoryCustomHeaders(t *testing.T) {
	data := map[string]string{"headers": customHeaders, "auth_type": HEADER_AUTH}
	credentialsFactory := NewCredentialsFactory(data)
	dbData := DBData{Services: Custom, IntegrationType: Webhook, MetaData: "MetaData"}

	credentials, err := credentialsFactory.NewCredentials(dbData)
	if err != nil {
		t.Logf("expected error: nil, got %v", err)
		t.Fail()
	}
	if credentials == nil {
		t.Logf("expected credentials, got nil")
		t.Fail()
	}
	switch credentials.(type) {
	case CustomHeaderCredentials:
		break
	default:
		t.Logf("Expected CustomHeadersCredentials, got %v", reflect.TypeOf(credentials))
		t.Fail()
	}
	if credentials.GetValues().HeaderJSONString != customHeaders {
		t.Logf("Expected header %v, got %v", customHeaders, credentials.GetValues())
		t.Fail()
	}
	if credentials.GetAuthType() != HEADER_AUTH {
		t.Logf("Expected type %v, got %v", HEADER_AUTH, credentials.GetAuthType())
		t.Fail()
	}
}

func TestGetCredentialsCustomHeaders(t *testing.T) {
	secret := &secrets.Secret{
		Name: "name",
		Type: "ssh",
		Data: appendKvs(&query.Kv{Key: "headers", Value: customHeaders}, &query.Kv{Key: "auth_type", Value: HEADER_AUTH}),
	}
	secretsClient := secrets.NewMockSecretsServiceClient(gomock.NewController(t))
	secretsClient.EXPECT().Read(
		context.Background(),
		gomock.Any(),
	).Return(secret, nil)

	credentials, err := GetCredentials(context.Background(), secretsClient, secretId, Custom, Webhook, "{}")

	if err != nil {
		t.Logf("Expected error: nil, got %v", err)
		t.Fail()
	}
	if credentials == nil {
		t.Log("Expected credentials, got nil")
		t.Fail()
	} else {
		if credentials.GetValues().HeaderJSONString != customHeaders {
			t.Logf("Expected header: %s, got %s", customHeaders, credentials.GetValues())
			t.Fail()
		}
		if credentials.GetAuthType() != HEADER_AUTH {
			t.Logf("Expected type %v, got %v", HEADER_AUTH, credentials.GetAuthType())
			t.Fail()
		}
	}
}

func TestNewCredentialsFactoryStorage(t *testing.T) {
	data := map[string]string{"access_key": accessKey, "secret_access_key": secretAccessKey}
	credentialsFactory := NewCredentialsFactory(data)
	dbData := DBData{Services: S3, IntegrationType: Storage, MetaData: `{"bucket":"` + bucket + `","region":"` + region + `"}`}
	credentials, err := credentialsFactory.NewCredentials(dbData)
	if err != nil {
		t.Logf("expected error: nil, got %v", err)
		t.Fail()
	}
	if credentials == nil {
		t.Logf("expected credentials, got nil")
		t.Fail()
	}
	switch credentials.(type) {
	case AwsCredentials:
		break
	default:
		t.Logf("Expected SplunkAuthCredentials, got %v", reflect.TypeOf(credentials))
		t.Fail()
	}
	if credentials.GetValues().AwsCreds.accesskey != accessKey {
		t.Logf("Expected header, got %v", credentials.GetValues().AwsCreds.accesskey)
		t.Fail()
	}
	if credentials.GetValues().AwsCreds.secretAccessKey != secretAccessKey {
		t.Logf("Expected header, got %v", credentials.GetValues().AwsCreds.secretAccessKey)
		t.Fail()
	}
	if credentials.GetValues().AwsCreds.bucket != bucket {
		t.Logf("Expected header, got %v", credentials.GetValues().AwsCreds.bucket)
		t.Fail()
	}
	if credentials.GetValues().AwsCreds.region != region {
		t.Logf("Expected header, got %v", credentials.GetValues().AwsCreds.region)
		t.Fail()
	}
}

func TestNewStorageCredentials(t *testing.T) {

	credentials := NewS3Credentials(accessKey, secretAccessKey, region, bucket)
	if credentials.accesskey != accessKey {
		t.Logf("expected username: %s, got: %s", accessKey, credentials.accesskey)
		t.Fail()
	}
	if credentials.secretAccessKey != secretAccessKey {
		t.Logf("expected username: %s, got: %s", secretAccessKey, credentials.secretAccessKey)
		t.Fail()
	}
	if credentials.bucket != bucket {
		t.Logf("expected username: %s, got: %s", bucket, credentials.bucket)
		t.Fail()
	}
	if credentials.region != region {
		t.Logf("expected username: %s, got: %s", region, credentials.region)
		t.Fail()
	}
}

func TesStorageCredentials(t *testing.T) {
	credentials := &AwsCredentials{accesskey: accessKey, secretAccessKey: secretAccessKey, region: region, bucket: bucket}
	if credentials.accesskey != accessKey {
		t.Logf("expected username: %s, got: %s", accessKey, credentials.accesskey)
		t.Fail()
	}
	if credentials.secretAccessKey != secretAccessKey {
		t.Logf("expected username: %s, got: %s", secretAccessKey, credentials.secretAccessKey)
		t.Fail()
	}
	if credentials.bucket != bucket {
		t.Logf("expected username: %s, got: %s", bucket, credentials.bucket)
		t.Fail()
	}
	if credentials.region != region {
		t.Logf("expected username: %s, got: %s", region, credentials.region)
		t.Fail()
	}
}
func TestGetCredentialsStorage(t *testing.T) {
	secret := &secrets.Secret{
		Name: "name",
		Type: "data-feed",
		Data: appendKvs(&query.Kv{Key: "accesskey", Value: accessKey}, &query.Kv{Key: "secretAccessKey", Value: secretAccessKey}),
	}
	secretsClient := secrets.NewMockSecretsServiceClient(gomock.NewController(t))
	secretsClient.EXPECT().Read(
		context.Background(),
		gomock.Any(),
	).Return(secret, nil)
	// need to Mock the secret using witout a splunk or basic auth
	credentials, err := GetCredentials(context.Background(), secretsClient, secretId, S3, Storage, `{"bucket":"`+bucket+`","region":"`+region+`"}`)
	if err != nil {
		t.Logf("Expected error: nil, got %v", err)
		t.Fail()
	}
	if credentials == nil {
		t.Log("Expected credentials, got nil")
		t.Fail()
	} else if credentials.GetValues().AwsCreds.bucket != bucket {
		t.Logf("Expected header, got %v", credentials.GetValues().AwsCreds.bucket)
		t.Fail()
	} else if credentials.GetValues().AwsCreds.region != region {
		t.Logf("Expected header, got %v", credentials.GetValues().AwsCreds.region)
		t.Fail()
	}
}
