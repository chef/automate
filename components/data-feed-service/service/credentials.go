package service

import (
	"context"
	"encoding/base64"

	"github.com/chef/automate/api/external/secrets"
	"github.com/pkg/errors"
)

const CredentialsError = "Could not create credentials"

type Credentials interface {
	GetAuthorizationHeaderValue() string
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

func (c BasicAuthCredentials) GetAuthorizationHeaderValue() string {
	return "Basic " + c.basicAuth()
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

func (c SplunkAuthCredentials) GetAuthorizationHeaderValue() string {
	return "Splunk " + c.splunkToken
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
