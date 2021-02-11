package integration_test

import (
	"context"
	"testing"

	"github.com/chef/automate/api/interservice/infra_proxy/request"
	"github.com/stretchr/testify/assert"
)

func TestGetClients(t *testing.T) {
	// Pre-populated that has been added by scripts
	// Validating the clients search based on these records.
	// rpc GetClients (request.Clients) returns (response.Clients)
	ctx := context.Background()
	req := &request.Clients{
		ServerId: autoDeployedChefServerID,
		OrgId:    autoDeployedChefOrganizationID,
	}

	t.Run("Clients list without a search params", func(t *testing.T) {
		// Default per_page 1000 is set in backend
		perPage := int32(1000)
		res, err := infraProxy.GetClients(ctx, req)
		assert.NoError(t, err)
		assert.NotNil(t, res)
		assert.Equal(t, int(res.Page), 0)
		if res.Total <= perPage {
			assert.Equal(t, int(res.Total), len(res.Clients))
		} else {
			assert.Equal(t, perPage, len(res.Clients))
		}
	})

	t.Run("Clients list with a per_page search param", func(t *testing.T) {
		req.SearchQuery = &request.SearchQuery{
			PerPage: 5,
		}
		res, err := infraProxy.GetClients(ctx, req)
		assert.NoError(t, err)
		assert.NotNil(t, res)
		assert.Equal(t, 0, int(res.Page))
		assert.Equal(t, 5, len(res.Clients))
	})

	t.Run("Clients list with a page search param", func(t *testing.T) {
		req.SearchQuery = &request.SearchQuery{
			Q:       "*:*",
			Page:    1,
			PerPage: 5,
		}
		res, err := infraProxy.GetClients(ctx, req)
		assert.NoError(t, err)
		assert.NotNil(t, res)
		assert.Equal(t, 1, int(res.Page))
		assert.Equal(t, 5, len(res.Clients))
	})

	t.Run("Clients list with an invalid query search param", func(t *testing.T) {
		req.SearchQuery = &request.SearchQuery{
			Q:       "NO_KEY:NO_VALUE",
			Page:    0,
			PerPage: 5,
		}
		res, err := infraProxy.GetClients(ctx, req)
		assert.NoError(t, err)
		assert.NotNil(t, res)
		assert.Equal(t, 0, int(res.Page))
		assert.Equal(t, 0, int(res.Total))
		assert.Equal(t, 0, len(res.Clients))
	})

	t.Run("Clients list with an invalid query search param", func(t *testing.T) {
		req.SearchQuery = &request.SearchQuery{
			Q:       "INVALID_QUERY",
			Page:    0,
			PerPage: 5,
		}
		res, err := infraProxy.GetClients(ctx, req)
		assert.NoError(t, err)
		assert.Equal(t, 0, int(res.Page))
		assert.Equal(t, 0, int(res.Total))
		assert.Equal(t, 0, len(res.Clients))
	})

	t.Run("Clients list with a valid query search param", func(t *testing.T) {
		req.SearchQuery = &request.SearchQuery{
			Q:       "name:chef-load-1",
			Page:    0,
			PerPage: 5,
		}
		res, err := infraProxy.GetClients(ctx, req)
		assert.NoError(t, err)
		assert.NotNil(t, res)
		assert.Equal(t, 0, int(res.Page))
		assert.Equal(t, 1, int(res.Total))
		assert.Equal(t, "chef-load-1", res.Clients[0].GetName())
	})

}

func TestGetClient(t *testing.T) {
	// rpc GetClient (request.Client) returns (response.Client)
	ctx := context.Background()
	req := &request.Client{
		ServerId: autoDeployedChefServerID,
		OrgId:    autoDeployedChefOrganizationID,
		Name:     "chef-load-1",
	}
	res, err := infraProxy.GetClient(ctx, req)
	assert.NoError(t, err)
	assert.NotNil(t, res)
	assert.Equal(t, "chef-load-1", res.Name)
}
