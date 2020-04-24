package server_test

import (
	"context"
	"testing"

	"github.com/stretchr/testify/require"
	healthpb "google.golang.org/grpc/health/grpc_health_v1"

	"github.com/chef/automate/components/infra-proxy-service/test"
)

func TestHealthGRPC(t *testing.T) {
	ctx := context.Background()
	_, _, conn, close, _, _ := test.SetupInfraProxyService(ctx, t)
	cl := healthpb.NewHealthClient(conn)

	defer close()

	t.Run("Check", func(t *testing.T) {
		actual, err := cl.Check(ctx, &healthpb.HealthCheckRequest{})
		require.NoError(t, err)
		require.Equal(t, healthpb.HealthCheckResponse_SERVING, actual.GetStatus())
	})
}
