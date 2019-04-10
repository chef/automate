package commands

import (
	"context"

	"github.com/chef/automate/components/data-feed-service/config"
	"github.com/chef/automate/components/data-feed-service/server"
	"github.com/chef/automate/lib/tracing"
	log "github.com/sirupsen/logrus"
	"github.com/spf13/cobra"
)

var serveCmd = &cobra.Command{
	Use:   "serve",
	Short: "Start the data feed gRPC server",
	Run: func(cmd *cobra.Command, args []string) {
		log.Debug("serve.go serveCmd ->")
		log.Info("Starting Data Feed Service...")

		closer, err := tracing.NewGlobalTracer("data-feed-service")
		if err != nil {
			log.WithError(err).Warn("Failed to start tracer for data-feed-service")
		}
		if closer != nil {
			defer tracing.CloseQuietly(closer)
		}

		cfg, err := config.Configure()
		if err != nil {
			log.WithError(err).Fatal("Failed to configure data-feed-service server")
		}

		err = server.StartGRPC(context.Background(), cfg)
		if err != nil {
			log.Fatalf("Failed to start GRPC server: %v", err)
		}

		log.Debug("end serve.go serveCmd ->")
	},
}
