package integration_test

import (
	"context"
	"fmt"
	"testing"
	"time"

	"github.com/chef/automate/api/interservice/infra_proxy/request"
	"github.com/stretchr/testify/assert"
)

func TestClientsReturnsEmptyList(t *testing.T) {
	// rpc GetGetServers (request.Clients) returns (response.Clients)

	testTimeStamp := time.Now().Second()
	err := suite.SetUpAutoDeployChefServer(testTimeStamp)
	assert.NoError(t, err)

	ctx := context.Background()
	req := &request.Clients{
		ServerId: "auto-deployed-test-server",
		OrgId:    fmt.Sprintf("auto-deployed-test-org-%d", testTimeStamp),
	}
	res, err := infraProxy.GetClients(ctx, req)
	assert.NoError(t, err)
	assert.NotNil(t, res)
	assert.Equal(t, 0, len(res.GetClients()))
}
