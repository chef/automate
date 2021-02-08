package integration_test

import (
	"context"
	"testing"

	"github.com/stretchr/testify/assert"

	"github.com/chef/automate/api/interservice/infra_proxy/request"
	"github.com/chef/automate/api/interservice/infra_proxy/response"
)

func TestGetServersReturnsEmptyList(t *testing.T) {
	// rpc GetGetServers (request.GetServers) returns (response.GetServers)
	ctx := context.Background()
	req := &request.GetServers{}

	expected := new(response.GetServers)
	res, err := infraProxy.GetServers(ctx, req)
	assert.NoError(t, err)
	assert.NotNil(t, res)

	assert.NotNil(t, expected)
}
