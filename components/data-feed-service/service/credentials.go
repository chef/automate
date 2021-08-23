package service

import (
	"context"
	"encoding/base64"
	"encoding/json"

	"github.com/chef/automate/api/external/secrets"
	"github.com/pkg/errors"
)

const CredentialsError = "Could not create credentials"

type AwsCredentials struct {
	accesskey       string
	secretAccessKey string
	region          string
	bucket          string
}

type AuthTypes struct {
	AuthorizationHeader string
	HeaderJSONString    string
	AwsCreds            AwsCredentials
}

type DBData struct {
	Services        string
	IntegrationType string
	MetaData        string
}

type Credentials interface {
	GetValues() AuthTypes
	GetAuthType() string
}

const (
	Custom       = "Custom"
	Webhook      = "Webhook"
	Storage      = "Storage"
	S3           = "S3"
	Minio        = "Minio"
	BASIC_AUTH   = "basic_auth"
	SPLUNK_AUTH  = "splunk_auth"
	HEADER_AUTH  = "header_auth"
	STORAGE_AUTH = "storage_auth"
)

type CredentialsFactory struct {
	data map[string]string
}

func NewCredentialsFactory(data map[string]string) *CredentialsFactory {
	return &CredentialsFactory{data}
}

func (c *CredentialsFactory) NewCredentials(data DBData) (Credentials, error) {
	if data.IntegrationType == Webhook || data.IntegrationType == "" {
		if username, ok := c.data["username"]; ok {
			// assume Basic Auth
			return NewBasicAuthCredentials(username, c.data["password"]), nil
		}
		if splunk, ok := c.data["Splunk"]; ok {
			return NewSplunkAuthCredentials(splunk), nil
		}
		if auth_type, ok := c.data["auth_type"]; ok {
			if auth_type == HEADER_AUTH {
				return NewCustomHeaderCredentials(c.data["headers"]), nil
			}
		}
	}
	if data.IntegrationType == Storage {
		if data.Services == S3 || data.Services == Minio {
			var zaMap map[string]string
			err := json.Unmarshal([]byte(data.MetaData), &zaMap)
			if err != nil {
				return nil, errors.Wrap(err, "unable to unmarshal map in NewCredentials")
			}
			return NewS3Credentials(c.data["access_key"], c.data["secret_access_key"], zaMap["region"], zaMap["bucket"]), nil
		}
	}
	return nil, errors.New(CredentialsError)
}

func NewS3Credentials(AccessKey string, secretAccessKey string, region string, bucket string) AwsCredentials {
	return AwsCredentials{accesskey: AccessKey, secretAccessKey: secretAccessKey, region: region, bucket: bucket}
}

func (c AwsCredentials) GetValues() AuthTypes {
	return AuthTypes{
		AwsCreds: AwsCredentials{
			accesskey:       c.accesskey,
			secretAccessKey: c.secretAccessKey,
			region:          c.region,
			bucket:          c.bucket,
		},
	}
}

func (c AwsCredentials) GetAuthType() string {
	return STORAGE_AUTH
}

type BasicAuthCredentials struct {
	username string
	password string
}

func NewBasicAuthCredentials(username string, password string) BasicAuthCredentials {
	return BasicAuthCredentials{username: username, password: password}
}
func (c BasicAuthCredentials) GetAuthType() string {
	return BASIC_AUTH
}

func (c BasicAuthCredentials) GetValues() AuthTypes {
	return AuthTypes{
		AuthorizationHeader: "Basic " + c.basicAuth(),
	}
}

func (c BasicAuthCredentials) basicAuth() string {
	auth := c.username + ":" + c.password
	return base64.StdEncoding.EncodeToString([]byte(auth))
}

type SplunkAuthCredentials struct {
	splunkToken string
}

func NewSplunkAuthCredentials(splunkToken string) SplunkAuthCredentials {
	return SplunkAuthCredentials{splunkToken: splunkToken}
}

func (c SplunkAuthCredentials) GetValues() AuthTypes {
	return AuthTypes{
		AuthorizationHeader: "Splunk " + c.splunkToken,
	}
}

func (c SplunkAuthCredentials) GetAuthType() string {
	return SPLUNK_AUTH
}

type CustomHeaderCredentials struct {
	headers string
}

func NewCustomHeaderCredentials(headers string) CustomHeaderCredentials {
	return CustomHeaderCredentials{headers: headers}
}

func (c CustomHeaderCredentials) GetValues() AuthTypes {
	return AuthTypes{
		HeaderJSONString: c.headers,
	}
}
func (c CustomHeaderCredentials) GetAuthType() string {
	return HEADER_AUTH
}

func GetCredentials(ctx context.Context, client secrets.SecretsServiceClient, secretID string, Services string, IntegrationType string, MetaData string) (Credentials, error) {
	secret, err := client.Read(ctx, &secrets.Id{Id: secretID})
	if err != nil {
		return nil, err
	}
	dbData := DBData{Services: Services, IntegrationType: IntegrationType, MetaData: MetaData}
	m := make(map[string]string)
	data := secret.GetData()
	for kv := range data {
		m[data[kv].Key] = data[kv].Value
	}

	credentialsFactory := NewCredentialsFactory(m)
	return credentialsFactory.NewCredentials(dbData)
}
