package integration_test

import (
	"context"
	"testing"

	"github.com/stretchr/testify/assert"

	"github.com/chef/automate/api/interservice/infra_proxy/request"
)

func TestClientsReturnsEmptyList(t *testing.T) {
	// rpc GetGetServers (request.Clients) returns (response.Clients)
	ctx := context.Background()
	err := suite.SetUpAutoDeployChefServer()
	req := &request.Clients{
		ServerId: "auto-deployed-test-server",
		OrgId:    "auto-deployed-test-org",
	}

	assert.NoError(t, err)

	res, err := infraProxy.GetClients(ctx, req)
	assert.NoError(t, err)
	assert.NotNil(t, res)
	assert.Equal(t, 0, len(res.GetClients()))
}
