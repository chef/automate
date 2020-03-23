package v1_test

import (
	"context"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
	"google.golang.org/grpc/codes"

	request "github.com/chef/automate/api/interservice/infra_proxy/request"
	infra_proxy "github.com/chef/automate/api/interservice/infra_proxy/service"
	"github.com/chef/automate/components/infra-proxy-service/constants"
	"github.com/chef/automate/components/infra-proxy-service/test"
	"github.com/chef/automate/lib/grpc/grpctest"
	"github.com/chef/automate/lib/logger"
)

func TestOrgs(t *testing.T) {
	ctx := context.Background()

	l, err := logger.NewLogger("text", "debug")
	require.NoError(t, err, "could not init logger", err)

	migrationConfig, err := test.MigrationConfigIfPGTestsToBeRun(l, "../../storage/postgres/migration/sql")
	require.NoError(t, err)

	_, serviceRef, conn, close, _ := test.SetupInfraProxyService(ctx, t, l, *migrationConfig)

	cl := infra_proxy.NewInfraProxyClient(conn)

	t.Helper()
	defer close()

	t.Run("CreateOrg", func(t *testing.T) {
		test.ResetState(context.Background(), t, serviceRef)

		t.Run("when a valid org is submitted", func(t *testing.T) {
			ctx := context.Background()

			serverRes, err := cl.CreateServer(ctx, &request.CreateServer{
				Name:        "Chef infra server",
				Description: "Chef infra server",
				Fqdn:        "domain.com",
				IpAddress:   "0.0.0.0",
			})
			require.NoError(t, err)
			require.NotNil(t, serverRes)

			req := &request.CreateOrg{
				Name:      "4thcoffee",
				AdminUser: "admin",
				AdminKey:  "--KEY--",
				ServerId:  serverRes.Server.Id,
				Projects:  []string{"project1", "project2"},
			}
			resp, err := cl.CreateOrg(ctx, req)
			require.NoError(t, err)
			require.NotNil(t, resp)
			org := resp.Org
			assert.Equal(t, req.Name, org.Name)
			assert.Equal(t, req.AdminUser, org.AdminUser)
			assert.Equal(t, req.AdminKey, org.AdminKey)
			assert.Equal(t, req.ServerId, org.ServerId)
			assert.Equal(t, 2, len(org.Projects))

			cleanupOrg(ctx, t, cl, resp.Org.Id)
			cleanupServer(ctx, t, cl, serverRes.Server.Id)
		})

		t.Run("when no projects are passed", func(t *testing.T) {
			ctx := context.Background()

			serverRes, err := cl.CreateServer(ctx, &request.CreateServer{
				Name:        "Chef infra server",
				Description: "Chef infra server",
				Fqdn:        "domain.com",
				IpAddress:   "0.0.0.0",
			})
			require.NoError(t, err)
			require.NotNil(t, serverRes)

			req := &request.CreateOrg{
				Name:      "4thcoffee",
				AdminUser: "admin",
				AdminKey:  "--KEY--",
				ServerId:  serverRes.Server.Id,
				Projects:  []string{},
			}

			resp, err := cl.CreateOrg(ctx, req)
			require.NoError(t, err)
			require.NotNil(t, resp)
			org := resp.Org
			assert.Equal(t, req.Name, org.Name)
			assert.Equal(t, 0, len(org.Projects))

			cleanupOrg(ctx, t, cl, resp.Org.Id)
			cleanupServer(ctx, t, cl, serverRes.Server.Id)
		})

		t.Run("when the  exists", func(t *testing.T) {
			ctx := context.Background()

			serverRes, err := cl.CreateServer(ctx, &request.CreateServer{
				Name:        "Chef infra server",
				Description: "Chef infra server",
				Fqdn:        "domain.com",
				IpAddress:   "0.0.0.0",
			})
			require.NoError(t, err)
			require.NotNil(t, serverRes)

			resp, err := cl.CreateOrg(ctx, &request.CreateOrg{
				Name:      "4thcoffee",
				AdminUser: "admin",
				AdminKey:  "--KEY--",
				ServerId:  serverRes.Server.Id,
				Projects:  []string{},
			})
			require.NoError(t, err)

			resp2, err := cl.CreateOrg(ctx, &request.CreateOrg{
				Name:      "4thcoffee",
				AdminUser: "admin",
				AdminKey:  "--KEY--",
				ServerId:  serverRes.Server.Id,
				Projects:  []string{},
			})
			assert.Nil(t, resp2)
			grpctest.AssertCode(t, codes.AlreadyExists, err)

			cleanupOrg(ctx, t, cl, resp.Org.Id)
			cleanupServer(ctx, t, cl, serverRes.Server.Id)
		})
	})

	t.Run("GetOrg", func(t *testing.T) {
		test.ResetState(context.Background(), t, serviceRef)

		t.Run("when there is no id in the request", func(t *testing.T) {
			ctx := context.Background()
			resp, err := cl.GetOrg(ctx, &request.GetOrg{
				Id: "",
			})

			require.Nil(t, resp)
			grpctest.AssertCode(t, codes.InvalidArgument, err)
		})

		t.Run("when the submitted ID is not a UUIDv4", func(t *testing.T) {
			ctx := context.Background()
			resp, err := cl.GetOrg(ctx, &request.GetOrg{
				Id: "35bffbab-3a49-dd8a-94a1-9ea87ec5c3cc",
			})

			require.Nil(t, resp)
			grpctest.AssertCode(t, codes.InvalidArgument, err)
		})

		t.Run("when the org does not exist", func(t *testing.T) {
			ctx := context.Background()
			resp, err := cl.GetOrg(ctx, &request.GetOrg{
				Id: "97e01ea1-976e-4626-88c8-43345c5d934f",
			})

			require.Nil(t, resp)
			grpctest.AssertCode(t, codes.NotFound, err)
		})

		t.Run("when the org exists", func(t *testing.T) {
			serverRes, err := cl.CreateServer(ctx, &request.CreateServer{
				Name:        "Chef infra server",
				Description: "Chef infra server",
				Fqdn:        "domain.com",
				IpAddress:   "0.0.0.0",
			})
			require.NoError(t, err)
			require.NotNil(t, serverRes)

			initResp, err := cl.CreateOrg(ctx, &request.CreateOrg{
				Name:      "infra_org",
				AdminUser: "admin_user",
				AdminKey:  "admin_key",
				ServerId:  serverRes.Server.Id,
				Projects:  []string{constants.UnassignedProjectID},
			})
			require.NoError(t, err)
			require.NotNil(t, initResp)

			resp, err := cl.GetOrg(ctx, &request.GetOrg{
				Id: initResp.Org.Id,
			})

			require.NoError(t, err)
			require.NotNil(t, resp)
			assert.Equal(t, initResp.Org.Name, resp.Org.Name)
			assert.Equal(t, initResp.Org.Id, resp.Org.Id)
			assert.Equal(t, initResp.Org.AdminUser, resp.Org.AdminUser)
			assert.Equal(t, initResp.Org.AdminKey, resp.Org.AdminKey)
			assert.Equal(t, initResp.Org.Projects, resp.Org.Projects)

			cleanupOrg(ctx, t, cl, initResp.Org.Id)
			cleanupServer(ctx, t, cl, serverRes.Server.Id)
		})
	})
}

func cleanupOrg(ctx context.Context, t *testing.T, cl infra_proxy.InfraProxyClient, orgID string) {
	t.Helper()
	deleteReq := request.DeleteOrg{Id: orgID}
	_, err := cl.DeleteOrg(ctx, &deleteReq)
	require.NoError(t, err)
}

func cleanupServer(ctx context.Context, t *testing.T, cl infra_proxy.InfraProxyClient, serverID string) {
	t.Helper()
	deleteReq := request.DeleteServer{Id: serverID}
	_, err := cl.DeleteServer(ctx, &deleteReq)
	require.NoError(t, err)
}
