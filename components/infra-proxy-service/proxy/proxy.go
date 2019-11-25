package proxy

import (
	"github.com/go-chef/chef"
	"github.com/pkg/errors"
)

// ChefClient is a V1 infra-proxy server
type ChefClient struct {
	client *chef.Client
}

// ChefConfig is a V1 infra-proxy server
type ChefConfig struct {
	Name    string
	Key     string
	SkipSSL bool
	BaseURL string
}

// NewChefClient is a V1 infra-proxy server
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
