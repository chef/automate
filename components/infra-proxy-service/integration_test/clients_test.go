package integration_test

import (
	"fmt"
	"testing"
	"time"

	"github.com/chef/automate/api/interservice/infra_proxy/request"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestGetClients(t *testing.T) {
	// Pre-populated that has been added by scripts
	// Validating the clients search based on these records.
	// rpc GetClients (request.Clients) returns (response.Clients)
	req := &request.Clients{
		ServerId: autoDeployedChefServerID,
		OrgId:    autoDeployedChefOrganizationID,
	}

	t.Run("Clients list without a search params", func(t *testing.T) {
		createReq1 := &request.CreateClient{
			ServerId:  autoDeployedChefServerID,
			OrgId:     autoDeployedChefOrganizationID,
			Name:      fmt.Sprintf("chef-load-%d", time.Now().Nanosecond()),
			CreateKey: true,
		}
		client1, err := infraProxy.CreateClient(ctx, createReq1)
		assert.NoError(t, err)
		assert.NotNil(t, client1)

		res, err := infraProxy.GetClients(ctx, req)
		assert.NoError(t, err)
		assert.NotNil(t, res)
		assert.Equal(t, int(res.Page), 0)
		assert.GreaterOrEqual(t, int(res.Total), 1)
	})

	t.Run("Clients list with a per_page search param", func(t *testing.T) {
		createReq1 := &request.CreateClient{
			ServerId:  autoDeployedChefServerID,
			OrgId:     autoDeployedChefOrganizationID,
			Name:      fmt.Sprintf("chef-load-%d", time.Now().Nanosecond()),
			CreateKey: true,
		}
		client1, err := infraProxy.CreateClient(ctx, createReq1)
		assert.NoError(t, err)
		assert.NotNil(t, client1)

		req.SearchQuery = &request.SearchQuery{
			PerPage: 1,
		}
		res, err := infraProxy.GetClients(ctx, req)
		assert.NoError(t, err)
		assert.NotNil(t, res)
		assert.Equal(t, 0, int(res.Page))
		assert.GreaterOrEqual(t, len(res.Clients), 1)
	})

	t.Run("Clients list with a page search param", func(t *testing.T) {
		createReq1 := &request.CreateClient{
			ServerId:  autoDeployedChefServerID,
			OrgId:     autoDeployedChefOrganizationID,
			Name:      fmt.Sprintf("chef-load-%d", time.Now().Nanosecond()),
			CreateKey: true,
		}
		client1, err := infraProxy.CreateClient(ctx, createReq1)
		assert.NoError(t, err)
		assert.NotNil(t, client1)

		createReq2 := &request.CreateClient{
			ServerId:  autoDeployedChefServerID,
			OrgId:     autoDeployedChefOrganizationID,
			Name:      fmt.Sprintf("chef-load-%d", time.Now().Nanosecond()),
			CreateKey: true,
		}
		client2, err := infraProxy.CreateClient(ctx, createReq2)
		assert.NoError(t, err)
		assert.NotNil(t, client2)

		req.SearchQuery = &request.SearchQuery{
			Q:       "*:*",
			Page:    1,
			PerPage: 1,
		}
		res, err := infraProxy.GetClients(ctx, req)
		assert.NoError(t, err)
		assert.NotNil(t, res)
		assert.Equal(t, 1, int(res.Page))
		assert.Equal(t, 1, len(res.Clients))
	})

	t.Run("Clients list with an invalid query search param", func(t *testing.T) {
		req.SearchQuery = &request.SearchQuery{
			Q:       "NO_KEY:NO_VALUE",
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
			Q:       "INVALID_QUERY:INVALID_QUERY",
			Page:    0,
			PerPage: 5,
		}
		res, err := infraProxy.GetClients(ctx, req)
		if err != nil {
			fmt.Println("error we got is err", err)
		}
		require.NoError(t, err)
		assert.Equal(t, 0, int(res.Page))
		assert.Equal(t, 0, int(res.Total))
		assert.Equal(t, 0, len(res.Clients))
	})

	// t.Run("Clients list with a valid query search param", func(t *testing.T) {
	// 	name := fmt.Sprintf("chef-load-%d", time.Now().Nanosecond())
	// 	createReq := &request.CreateClient{
	// 		ServerId:  autoDeployedChefServerID,
	// 		OrgId:     autoDeployedChefOrganizationID,
	// 		Name:      name,
	// 		Validator: true,
	// 		CreateKey: true,
	// 	}
	// 	client, err := infraProxy.CreateClient(ctx, createReq)
	// 	assert.NoError(t, err)
	// 	assert.NotNil(t, client)

	// 	req.SearchQuery = &request.SearchQuery{
	// 		Q:       fmt.Sprintf("name:%s", name),
	// 		Page:    0,
	// 		PerPage: 5,
	// 	}
	// 	res, err := infraProxy.GetClients(ctx, req)
	// 	assert.NoError(t, err)
	// 	assert.NotNil(t, res)
	// 	assert.Equal(t, 0, int(res.Page))
	// 	assert.Equal(t, 1, int(res.Total))
	// 	assert.Equal(t, name, res.Clients[0].GetName())
	// 	assert.Equal(t, true, res.Clients[0].GetValidator())
	// })

}

func TestGetClient(t *testing.T) {
	// rpc GetClient (request.Client) returns (response.Client)
	name := fmt.Sprintf("chef-load-%d", time.Now().Nanosecond())
	createReq := &request.CreateClient{
		ServerId:  autoDeployedChefServerID,
		OrgId:     autoDeployedChefOrganizationID,
		Name:      name,
		CreateKey: true,
	}
	res, err := infraProxy.CreateClient(ctx, createReq)
	assert.NoError(t, err)
	assert.NotNil(t, res)

	req := &request.Client{
		ServerId: autoDeployedChefServerID,
		OrgId:    autoDeployedChefOrganizationID,
		Name:     name,
	}
	client, err := infraProxy.GetClient(ctx, req)
	assert.NoError(t, err)
	assert.NotNil(t, client)
	assert.Equal(t, name, client.GetName())
	assert.Equal(t, false, client.GetValidator())
}

func TestResetClient(t *testing.T) {
	// rpc ResetClient (request.ClientKey) returns (response.ResetClient)
	name := fmt.Sprintf("client-%d", time.Now().Nanosecond())
	req := &request.CreateClient{
		ServerId:  autoDeployedChefServerID,
		OrgId:     autoDeployedChefOrganizationID,
		Name:      name,
		CreateKey: true,
	}
	res, err := infraProxy.CreateClient(ctx, req)
	assert.NoError(t, err)
	assert.NotNil(t, res)

	resetRes, err := infraProxy.ResetClientKey(ctx, &request.ClientKey{
		ServerId: autoDeployedChefServerID,
		OrgId:    autoDeployedChefOrganizationID,
		Name:     name,
	})
	assert.NoError(t, err)
	assert.NotNil(t, resetRes)
	assert.Equal(t, res.GetName(), resetRes.GetName())
	// The old private key must not match with the reset private key
	assert.NotEqual(t, res.GetClientKey().GetPrivateKey(), resetRes.GetClientKey().GetPrivateKey())
}

// Add client records
func addClients(n int) int {
	total := 0
	for i := 0; i < n; i++ {
		req := &request.CreateClient{
			ServerId:  autoDeployedChefServerID,
			OrgId:     autoDeployedChefOrganizationID,
			Name:      fmt.Sprintf("chef-load-client-%d", time.Now().Nanosecond()),
			CreateKey: true,
		}
		_, err := infraProxy.CreateClient(ctx, req)

		if err == nil {
			total++
		}
	}

	return total
}
