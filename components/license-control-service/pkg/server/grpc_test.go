package server_test

import (
	"context"
	"os"
	"testing"

	log "github.com/sirupsen/logrus"
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
	if err != nil {
		t.Fatalf("Error reading config file: %s", err)
	}

	cfg, err := server.ConfigFromViper()
	if err != nil {
		t.Fatalf("Failed to configure service: %s", err)
	}

	g := grpctest.NewServer(server.NewGRPC(ctx, cfg))
	defer g.Close()

	connFactory := secureconn.NewFactory(*cfg.ServiceCerts)
	conn, err := connFactory.Dial("license-control-service", cfg.ListenAddress())
	if err != nil {
		log.WithFields(
			log.Fields{
				"license_control_service_address": cfg.ListenAddress(),
				"error":                           err,
			},
		).Error("Unable to connect to license-control-service")
		os.Exit(1)
	}

	cl := healthpb.NewHealthClient(conn)
	t.Run("Check", func(t *testing.T) {
		actual, err := cl.Check(ctx, &healthpb.HealthCheckRequest{})
		require.NoError(t, err, "No error failed")
		assert.Equal(t, healthpb.HealthCheckResponse_SERVING, actual.GetStatus(), "Assert failed")
	})
}
