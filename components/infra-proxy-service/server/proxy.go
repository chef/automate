package server

import (
	"context"
	"net/url"

	chef "github.com/go-chef/chef"
	"github.com/pkg/errors"
	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/status"

	secrets "github.com/chef/automate/api/external/secrets"
	"github.com/chef/automate/components/infra-proxy-service/service"
)

// ChefConfig is an infra-proxy server
type ChefConfig struct {
	Name    string
	Key     string
	SkipSSL bool
	BaseURL string
}

// ChefClient type definition for the chef client
type ChefClient struct {
	client *chef.Client
}

// NewChefClient is an infra-proxy server
func NewChefClient(config *ChefConfig) (*ChefClient, error) {

	// build a client
	client, err := chef.NewClient(&chef.Config{
		Name:    config.Name,
		Key:     config.Key,
		SkipSSL: config.SkipSSL,
		BaseURL: config.BaseURL,
	})

	if err != nil {
		return nil, errors.Wrap(err, err.Error())
	}

	return &ChefClient{client: client}, nil
}

func (s *Server) createClient(ctx context.Context, orgID string, serverID string) (*ChefClient, error) {
	ctx, cancel := context.WithCancel(ctx)
	defer cancel()

	// TODO: combine get server & org query in one statement.
	server, err := s.service.Storage.GetServer(ctx, serverID)
	if err != nil {
		return nil, service.ParseStorageError(err, serverID, "server")
	}

	org, err := s.service.Storage.GetOrg(ctx, orgID, serverID)
	if err != nil {
		return nil, service.ParseStorageError(err, orgID, "org")
	}

	secret, err := s.service.Secrets.Read(ctx, &secrets.Id{Id: org.CredentialID})
	if err != nil {
		return nil, err
	}

	baseURL, err := targetURL(server.Fqdn, server.IPAddress, org.Name)
	if err != nil {
		return nil, status.Errorf(codes.InvalidArgument, "invalid server url: %s", baseURL)
	}

	client, err := NewChefClient(&ChefConfig{
		Name:    org.AdminUser,
		Key:     GetOrgAdminKeyFrom(secret),
		SkipSSL: true,
		BaseURL: baseURL,
	})

	return client, err
}

// targetURL is constructing the base URL based on fqdn|ipAddress value
func targetURL(fqdn string, IPAddress string, orgName string) (string, error) {
	path := fqdn
	if path == "" {
		path = IPAddress
	}
	path = path + "/organizations/" + orgName + "/"

	baseURL, err := url.Parse(path)
	if err != nil {
		return "", errors.Wrap(err, err.Error())
	}
	baseURL.Scheme = "https"

	return baseURL.String(), nil
}

// GetOrgAdminKeyFrom returns AdminKey
func GetOrgAdminKeyFrom(secret *secrets.Secret) string {
	adminKey := ""
	if secret != nil {
		for _, item := range secret.Data {
			if item.Key == "key" {
				adminKey = item.Value
			}
		}
	}

	return adminKey
}
