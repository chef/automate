package server_test

import (
	"context"
	"testing"

	"github.com/stretchr/testify/require"

	"github.com/chef/automate/api/external/common/version"
	infra_proxy "github.com/chef/automate/api/interservice/infra_proxy/service"
	"github.com/chef/automate/lib/logger"

	"github.com/chef/automate/components/infra-proxy-service/test"
)

func TestVersion(t *testing.T) {
	ctx := context.Background()

	l, err := logger.NewLogger("text", "debug")
	require.NoError(t, err, "could not init logger", err)

	migrationConfig, err := test.MigrationConfigIfPGTestsToBeRun(l, "../storage/postgres/migration/sql")
	require.NoError(t, err)

	_, _, conn, close, _ := test.SetupInfraProxyService(ctx, t, l, *migrationConfig)
	defer close()

	cl := infra_proxy.NewInfraProxyClient(conn)

	t.Helper()
	defer close()

	t.Run("GetVersion", func(t *testing.T) {
		resp, err := cl.GetVersion(ctx, &version.VersionInfoRequest{})
		require.NoError(t, err)
		// Version is injected via linker flags that we don't set during tests
		require.Equal(t, "unknown", resp.Version)
	})
}
