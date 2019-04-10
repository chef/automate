package grpc

import (
	"context"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
	healthpb "google.golang.org/grpc/health/grpc_health_v1"

	"github.com/chef/automate/components/secrets-service/config"
	"github.com/chef/automate/components/secrets-service/dao"
	"github.com/chef/automate/lib/grpc/grpctest"
	"github.com/chef/automate/lib/grpc/secureconn"
	"github.com/chef/automate/lib/tls/test/helpers"
)

func TestHealthGRPC(t *testing.T) {
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	serviceCerts := helpers.LoadDevCerts(t, "secrets-service")

	connFactory := secureconn.NewFactory(*serviceCerts)
	postgresConfig := config.Postgres{ConnectionString: "", MigrationsPath: ""}
	db, _ := dao.New(&postgresConfig, "75e79c17ae62445e9771cd13fc4216f4")
	grpcServer := NewGRPCServer(db, connFactory)

	g := grpctest.NewServer(grpcServer)
	defer g.Close()

	conn, err := connFactory.Dial("secrets-service", g.URL)
	if err != nil {
		t.Fatalf("connecting to grpc endpoint: %s", err)
	}
	cl := healthpb.NewHealthClient(conn)

	t.Run("Check", func(t *testing.T) {
		actual, err := cl.Check(ctx, &healthpb.HealthCheckRequest{})
		require.NoError(t, err)
		assert.Equal(t, healthpb.HealthCheckResponse_SERVING, actual.GetStatus())
	})
}
