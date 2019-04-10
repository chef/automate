package grpcserver_test

import (
	"context"
	"testing"

	"github.com/chef/automate/api/interservice/cfgmgmt/request"
	m "github.com/chef/automate/components/config-mgmt-service/backend/mock"
	"github.com/chef/automate/components/config-mgmt-service/config"
	"github.com/chef/automate/components/config-mgmt-service/grpcserver"

	"github.com/stretchr/testify/assert"
)

// This test is using the Mock backend client
// so that we can easily test our routes.
func TestStatsRoutes(t *testing.T) {
	cfg := config.Default()
	cfg.SetBackend(m.New())
	server := grpcserver.NewCfgMgmtServer(cfg)

	nodesCounts, err := server.GetNodesCounts(context.Background(), &request.NodesCounts{})

	assert.Nil(t, err)
	assert.Equal(t, int32(0), nodesCounts.Success)
	assert.Equal(t, int32(0), nodesCounts.Failure)
	assert.Equal(t, int32(0), nodesCounts.Missing)
	assert.Equal(t, int32(0), nodesCounts.Total)
}

func TestStatsRunsRoutes(t *testing.T) {
	cfg := config.Default()
	cfg.SetBackend(m.New())
	server := grpcserver.NewCfgMgmtServer(cfg)

	runsCounts, err := server.GetRunsCounts(context.Background(), &request.RunsCounts{})

	assert.Nil(t, err)
	assert.Equal(t, int32(0), runsCounts.Success)
	assert.Equal(t, int32(0), runsCounts.Failure)
	assert.Equal(t, int32(0), runsCounts.Total)
}
