package commands

import (
	"context"

	log "github.com/sirupsen/logrus"
	"github.com/spf13/cobra"

	"github.com/chef/automate/components/license-control-service/pkg/server"
	"github.com/chef/automate/lib/tracing"
)

var serveCmd = &cobra.Command{
	Use:   "serve",
	Short: "Start the license control gRPC server",
	Run: func(*cobra.Command, []string) {
		log.Info("Starting License Control Service")

		cfg, err := server.ConfigFromViper()
		if err != nil {
			log.WithError(err).Fatal("Failed to configure service")
		}

		closer, err := tracing.NewGlobalTracer("license-control-service")
		if err != nil {
			log.WithError(err).Warn("Failed to start tracer for license-control-service")
		}
		defer tracing.CloseQuietly(closer)

		err = server.StartGRPC(context.Background(), cfg)
		if err != nil {
			log.Fatal(err)
		}
	},
}

func init() {
	RootCmd.AddCommand(serveCmd)
}
