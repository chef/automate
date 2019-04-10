package commands

import (
	"context"

	"fmt"

	"github.com/chef/automate/components/event-service/config"
	"github.com/chef/automate/components/event-service/nats"
	"github.com/chef/automate/components/event-service/server"
	"github.com/chef/automate/lib/tracing"
	log "github.com/sirupsen/logrus"
	"github.com/spf13/cobra"
)

func newServeCmd() *cobra.Command {
	return &cobra.Command{
		Use:   "serve",
		Short: "Start the event gRPC server",
		Run: func(cmd *cobra.Command, args []string) {

			cfg, err := config.Configure()
			if err != nil {
				log.WithError(err).Fatal("Failed to configure event-service server")
			}
			uri := fmt.Sprintf("%s:%d", cfg.ServiceConfig.Host, cfg.ServiceConfig.Port)
			log.WithFields(log.Fields{"uri": uri}).Info("Starting Event Service...")

			closer, err := tracing.NewGlobalTracer("event-service")
			if err != nil {
				log.WithError(err).Warn("Failed to start tracer for event-service")
			}
			if closer != nil {
				defer tracing.CloseQuietly(closer)
			}

			// Start a NATs Server only if the feature was enabled through the config
			// @afiune should this replace the event-service?
			if cfg.StreamService.Enabled {
				go func() {
					err := nats.Spawn(cfg)
					if err != nil {
						log.Fatalf("failed starting NATS: %+v", err)
					}
				}()
			}

			server.StartGRPC(context.Background(), cfg)
		},
	}

}
