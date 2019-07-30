package server_test

import (
	"context"
	"os"
	"testing"

	"github.com/spf13/viper"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
	healthpb "google.golang.org/grpc/health/grpc_health_v1"

	"github.com/chef/automate/components/license-control-service/pkg/server"
	"github.com/chef/automate/lib/grpc/grpctest"
	"github.com/chef/automate/lib/grpc/secureconn"
)

func TestHealthCheck(t *testing.T) {
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	viper.SetConfigFile("./testdata/config.toml")

	err := viper.ReadInConfig()
	require.NoError(t, err, "reading config file")

	cfg, err := server.ConfigFromViper()
	require.NoError(t, err, "configuring service")
	cfg.PGURL = os.Getenv("PG_URL")
	if cfg.PGURL == "" {
		t.Fatal("test requires PG_URL to be set")
	}

	srv, err := server.NewGRPC(ctx, cfg)
	require.NoError(t, err, "initializing grpc server")

	g := grpctest.NewServer(srv)
	defer g.Close()

	connFactory := secureconn.NewFactory(*cfg.ServiceCerts)
	conn, err := connFactory.Dial("license-control-service", g.URL)
	require.NoError(t, err, "dialing license-control-service")

	cl := healthpb.NewHealthClient(conn)
	t.Run("Check", func(t *testing.T) {
		actual, err := cl.Check(ctx, &healthpb.HealthCheckRequest{})
		require.NoError(t, err)
		assert.Equal(t, healthpb.HealthCheckResponse_SERVING, actual.GetStatus())
	})
}
