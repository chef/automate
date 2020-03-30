package server_test

import (
	"context"
	"testing"

	"github.com/stretchr/testify/require"
	healthpb "google.golang.org/grpc/health/grpc_health_v1"

	"github.com/chef/automate/lib/logger"

	"github.com/chef/automate/components/infra-proxy-service/test"
)

func TestHealthGRPC(t *testing.T) {
	ctx := context.Background()

	l, err := logger.NewLogger("text", "debug")
	require.NoError(t, err, "could not init logger", err)

	migrationConfig, err := test.MigrationConfigIfPGTestsToBeRun(l, "../storage/postgres/migration/sql")
	require.NoError(t, err)

	_, _, conn, close, _ := test.SetupInfraProxyService(ctx, t, l, *migrationConfig)
	defer close()

	cl := healthpb.NewHealthClient(conn)

	t.Run("Check", func(t *testing.T) {
		actual, err := cl.Check(ctx, &healthpb.HealthCheckRequest{})
		require.NoError(t, err)
		require.Equal(t, healthpb.HealthCheckResponse_SERVING, actual.GetStatus())
	})
}
