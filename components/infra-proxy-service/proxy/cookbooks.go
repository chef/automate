package proxy

import (
	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/status"

	"github.com/go-chef/chef"
)

// GetCookbooks get cookbooks list
func (c *ChefClient) GetCookbooks() (chef.CookbookListResult, error) {
	cookList, err := c.client.Cookbooks.List()
	if err != nil {
		return nil, status.Error(codes.InvalidArgument, err.Error())
	}

	return cookList, nil
}

// GetCookbooksAvailableVersions get cookbooks list
func (c *ChefClient) GetCookbooksAvailableVersions(numVersions string) (chef.CookbookListResult, error) {
	cookList, err := c.client.Cookbooks.ListAvailableVersions(numVersions)
	if err != nil {
		return nil, status.Error(codes.InvalidArgument, err.Error())
	}

	return cookList, nil
}

// GetCookbook get cookbooks list
func (c *ChefClient) GetCookbook(name string, version string) (chef.CookbookVersion, error) {
	cookbook, err := c.client.Cookbooks.Get(name)
	if err != nil {
		return cookbook, status.Error(codes.InvalidArgument, err.Error())
	}

	return cookbook, nil
}
