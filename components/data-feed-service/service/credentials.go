package service

import (
	"context"
	"encoding/base64"

	"github.com/chef/automate/api/external/secrets"
	"github.com/pkg/errors"
)

const CredentialsError = "Could not create credentials"
const (
	BASIC_AUTH  = "basic_auth"
	SPLUNK_AUTH = "splunk_auth"
	HEADER_AUTH = "header_auth"
	AWS_AUTH    = "aws_auth"
)

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

type Credentials interface {
	GetValues() AuthTypes
	GetAuthType() string
}

type CredentialsFactory struct {
	data map[string]string
}

func NewCredentialsFactory(data map[string]string) *CredentialsFactory {
	return &CredentialsFactory{data}
}

func (c *CredentialsFactory) NewCredentials() (Credentials, error) {
	if username, ok := c.data["username"]; ok {
		// assume Basic Auth
		return NewBasicAuthCredentials(username, c.data["password"]), nil
	}
	if splunk, ok := c.data["Splunk"]; ok {
		return NewSplunkAuthCredentials(splunk), nil
	}
	return nil, errors.New(CredentialsError)
}

type BasicAuthCredentials struct {
	username string
	password string
}

func NewBasicAuthCredentials(username string, password string) BasicAuthCredentials {
	return BasicAuthCredentials{username: username, password: password}
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

func (c BasicAuthCredentials) GetAuthType() string {
	return BASIC_AUTH
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

func GetCredentials(ctx context.Context, client secrets.SecretsServiceClient, secretID string) (Credentials, error) {
	secret, err := client.Read(ctx, &secrets.Id{Id: secretID})
	if err != nil {
		return nil, err
	}

	m := make(map[string]string)
	data := secret.GetData()
	for kv := range data {
		m[data[kv].Key] = data[kv].Value
	}

	credentialsFactory := NewCredentialsFactory(m)
	return credentialsFactory.NewCredentials()
}
