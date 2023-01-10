package integration_test

import (
	"testing"

	"github.com/chef/automate/api/interservice/infra_proxy/request"
	"github.com/stretchr/testify/assert"
)

func TestGetCookBooks(t *testing.T) {

	t.Run("Cookbooks list without a search params", func(t *testing.T) {
		req := &request.Cookbooks{
			ServerId: autoDeployedChefServerID,
			OrgId:    autoDeployedChefOrganizationID,
		}
		res, err := infraProxy.GetCookbooks(ctx, req)
		assert.NoError(t, err)
		assert.NotNil(t, res)

	})

	t.Run("Get CookBook List with no org and server ID with error", func(t *testing.T) {
		req := &request.Cookbooks{}
		res, err := infraProxy.GetCookbooks(ctx, req)
		assert.Error(t, err)
		assert.Nil(t, res)

	})

	t.Run("Get Cookbook information with Version", func(t *testing.T) {
		req := &request.Cookbook{
			ServerId: autoDeployedChefServerID,
			OrgId:    autoDeployedChefOrganizationID,
			Name:     "audit",
			Version:  "9.5.0",
		}
		res, err := infraProxy.GetCookbook(ctx, req)
		assert.NoError(t, err)
		assert.NotNil(t, res)
		assert.Equal(t, "audit-9.5.0", res.Name)
	})

	t.Run("Get Cookbook information without Version", func(t *testing.T) {
		req := &request.Cookbook{
			ServerId: autoDeployedChefServerID,
			OrgId:    autoDeployedChefOrganizationID,
			Name:     "audit",
		}
		res, err := infraProxy.GetCookbook(ctx, req)
		assert.NoError(t, err)
		assert.NotNil(t, res)
		assert.Equal(t, "audit-9.5.0", res.Name)
	})

	t.Run("Get Cookbook information without Name", func(t *testing.T) {
		req := &request.Cookbook{
			ServerId: autoDeployedChefServerID,
			OrgId:    autoDeployedChefOrganizationID,
			Version:  "9.5.0",
		}
		res, err := infraProxy.GetCookbook(ctx, req)
		assert.Error(t, err)
		assert.Nil(t, res)

	})

	t.Run("Get Cookbook Versions with CookBook Name", func(t *testing.T) {
		req := &request.CookbookVersions{
			ServerId: autoDeployedChefServerID,
			OrgId:    autoDeployedChefOrganizationID,
			Name:     "audit",
		}
		res, err := infraProxy.GetCookbookVersions(ctx, req)
		assert.NoError(t, err)
		assert.NotNil(t, res)
		assert.Equal(t, 1, len(res.Versions))
	})

	t.Run("Get Cookbook Versions without CookBook Name", func(t *testing.T) {
		req := &request.CookbookVersions{
			ServerId: autoDeployedChefServerID,
			OrgId:    autoDeployedChefOrganizationID,
		}
		res, err := infraProxy.GetCookbookVersions(ctx, req)
		assert.Error(t, err)
		assert.Nil(t, res)

	})

}
