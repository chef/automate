package integration_test

import (
	"context"
	"testing"

	"github.com/chef/automate/api/interservice/infra_proxy/request"
	"github.com/stretchr/testify/assert"
)

func TestGetClients(t *testing.T) {
	// rpc GetClients (request.Clients) returns (response.Clients)
	ctx := context.Background()
	req := &request.Clients{
		ServerId: autoDeployedChefServerID,
		OrgId:    autoDeployedChefOrganizationID,
	}
	res, err := infraProxy.GetClients(ctx, req)
	assert.NoError(t, err)
	assert.NotNil(t, res)
	assert.Equal(t, 3, len(res.GetClients()))
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
