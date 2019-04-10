package health_test

import (
	"context"
	"testing"

	"google.golang.org/grpc/codes"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
	"google.golang.org/grpc"
	healthpb "google.golang.org/grpc/health/grpc_health_v1"

	"github.com/chef/automate/lib/grpc/grpctest"
	"github.com/chef/automate/lib/grpc/health"
)

func TestHealthService(t *testing.T) {
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	s := grpc.NewServer()

	h := health.NewService()
	health.RegisterHealthServer(s, h)
	g := grpctest.NewServer(s)

	conn, err := grpc.DialContext(ctx, g.URL, grpc.WithInsecure())
	if err != nil {
		t.Fatalf("connecting to grpc endpoint: %s", err)
	}
	cl := healthpb.NewHealthClient(conn)

	t.Run("Check", func(t *testing.T) {
		t.Run("when called without service name, returns serving", func(t *testing.T) {
			actual, err := cl.Check(ctx, &healthpb.HealthCheckRequest{})
			require.NoError(t, err)
			assert.Equal(t, healthpb.HealthCheckResponse_SERVING, actual.GetStatus())
		})

		t.Run("when called with unknown service name, returns not found", func(t *testing.T) {
			actual, err := cl.Check(ctx, &healthpb.HealthCheckRequest{Service: "foo"})
			grpctest.AssertCode(t, codes.NotFound, err)
			assert.Equal(t, healthpb.HealthCheckResponse_UNKNOWN, actual.GetStatus())
		})

		t.Run("when called with known service name, returns its status", func(t *testing.T) {
			h.SetServingStatus("bar", healthpb.HealthCheckResponse_NOT_SERVING)
			actual, err := cl.Check(ctx, &healthpb.HealthCheckRequest{Service: "bar"})
			require.NoError(t, err)
			assert.Equal(t, healthpb.HealthCheckResponse_NOT_SERVING, actual.GetStatus())
		})
	})
}
