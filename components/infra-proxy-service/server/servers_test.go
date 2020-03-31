package server_test

import (
	"context"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
	"google.golang.org/grpc/codes"

	request "github.com/chef/automate/api/interservice/infra_proxy/request"
	infra_proxy "github.com/chef/automate/api/interservice/infra_proxy/service"
	"github.com/chef/automate/components/infra-proxy-service/test"
	"github.com/chef/automate/lib/grpc/grpctest"
	"github.com/chef/automate/lib/logger"
)

func TestServers(t *testing.T) {
	ctx := context.Background()

	l, err := logger.NewLogger("text", "debug")
	require.NoError(t, err, "could not init logger", err)

	migrationConfig, err := test.MigrationConfigIfPGTestsToBeRun(l, "../storage/postgres/migration/sql")
	require.NoError(t, err)

	_, serviceRef, conn, close, _ := test.SetupInfraProxyService(ctx, t, l, *migrationConfig)

	cl := infra_proxy.NewInfraProxyClient(conn)

	t.Helper()
	defer close()

	t.Run("CreateServer", func(t *testing.T) {
		test.ResetState(ctx, t, serviceRef)

		t.Run("when a valid server is submitted, creates the new server successfully", func(t *testing.T) {
			ctx := context.Background()

			req := &request.CreateServer{
				Name:        "chef-infra-server",
				Description: "Chef infra server",
				Fqdn:        "domain.com",
				IpAddress:   "0.0.0.0",
			}
			resp, err := cl.CreateServer(ctx, req)
			require.NoError(t, err)
			require.NotNil(t, resp)
			assert.Equal(t, req.Name, resp.Server.Name)
			assert.Equal(t, req.Description, resp.Server.Description)
			assert.Equal(t, req.Fqdn, resp.Server.Fqdn)
			assert.Equal(t, req.IpAddress, resp.Server.IpAddress)

			cleanupServer(ctx, t, cl, resp.Server.Id)
		})

		t.Run("when name is missing, raise invalid argument error", func(t *testing.T) {
			ctx := context.Background()

			req := &request.CreateServer{
				Description: "Chef infra server",
				Fqdn:        "domain.com",
				IpAddress:   "0.0.0.0",
			}
			resp, err := cl.CreateServer(ctx, req)
			assert.Nil(t, resp)
			grpctest.AssertCode(t, codes.InvalidArgument, err)
		})

		t.Run("when name is already exist, raise invalid argument error", func(t *testing.T) {
			ctx := context.Background()

			req1 := &request.CreateServer{
				Name:        "chef-infra-server",
				Description: "Chef infra server",
				Fqdn:        "domain.com",
				IpAddress:   "0.0.0.0",
			}
			resp1, err := cl.CreateServer(ctx, req1)
			require.NoError(t, err)
			require.NotNil(t, resp1)
			assert.Equal(t, req1.Name, resp1.Server.Name)

			req2 := &request.CreateServer{
				Name:        "chef-infra-server",
				Description: "New chef infra server",
				Fqdn:        "domain.com",
				IpAddress:   "0.0.0.0",
			}
			resp2, err := cl.CreateServer(ctx, req2)
			assert.Nil(t, resp2)
			grpctest.AssertCode(t, codes.AlreadyExists, err)

			cleanupServer(ctx, t, cl, resp1.Server.Id)
		})

		t.Run("when description is missing, raise invalid argument error", func(t *testing.T) {
			ctx := context.Background()

			req := &request.CreateServer{
				Name:      "chef-infra-server",
				Fqdn:      "domain.com",
				IpAddress: "0.0.0.0",
			}
			resp, err := cl.CreateServer(ctx, req)
			assert.Nil(t, resp)
			grpctest.AssertCode(t, codes.InvalidArgument, err)
		})

		t.Run("when fqdn is missing, raise invalid argument error", func(t *testing.T) {
			ctx := context.Background()

			req := &request.CreateServer{
				Name:        "chef-infra-server",
				Description: "Chef infra server",
				IpAddress:   "0.0.0.0",
			}
			resp, err := cl.CreateServer(ctx, req)
			assert.Nil(t, resp)
			grpctest.AssertCode(t, codes.InvalidArgument, err)
		})

		t.Run("when ipAddress is missing, raise invalid argument error", func(t *testing.T) {
			ctx := context.Background()

			req := &request.CreateServer{
				Name:        "chef-infra-server",
				Description: "Chef infra server",
				Fqdn:        "domain.com",
			}
			resp, err := cl.CreateServer(ctx, req)
			assert.Nil(t, resp)
			grpctest.AssertCode(t, codes.InvalidArgument, err)
		})
	})

	t.Run("GetServers", func(t *testing.T) {
		test.ResetState(context.Background(), t, serviceRef)
	})

	t.Run("GetServer", func(t *testing.T) {
		test.ResetState(context.Background(), t, serviceRef)
	})

	t.Run("DeleteServer", func(t *testing.T) {
		test.ResetState(context.Background(), t, serviceRef)
	})

	t.Run("UpdateServer", func(t *testing.T) {
		test.ResetState(context.Background(), t, serviceRef)
	})
}
