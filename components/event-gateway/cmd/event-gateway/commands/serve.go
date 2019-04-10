package commands

import (
	"fmt"
	"time"

	log "github.com/sirupsen/logrus"
	"github.com/spf13/cobra"

	"github.com/chef/automate/components/event-gateway/pkg/config"
	"github.com/chef/automate/components/event-gateway/pkg/nats"
	"github.com/chef/automate/lib/tracing"
)

func newServeCmd() *cobra.Command {
	return &cobra.Command{
		Use:   "serve",
		Short: "Start the event-gateway (NATS) server",
		Run: func(cmd *cobra.Command, args []string) {
			// set the config file if it's given
			log.Infof("Config file is %q", cfgFile)

			cfg, err := config.Configure()
			log.WithFields(log.Fields{
				"level": cfg.LogConfig.LogLevel,
			}).Info("log level set")
			if err != nil {
				log.WithError(err).Fatal("Failed to configure event-gateway server")
			}
			uri := fmt.Sprintf("nats://%s:%d", cfg.Service.Host, cfg.Service.Port)
			log.WithFields(log.Fields{"uri": uri}).Info("Starting Event Gateway...")

			closer, err := tracing.NewGlobalTracer("event-gateway")
			if err != nil {
				log.WithError(err).Warn("Failed to start tracer for event-gateway")
			}
			if closer != nil {
				defer tracing.CloseQuietly(closer)
			}

			// Start a NATs Server only if the feature was enabled through the config
			if !cfg.Service.Enabled {
				for {
					time.Sleep(1 * time.Minute)
				}
			}

			err = nats.GenerateHealthCheckCredentials(cfg)
			if err != nil {
				log.Fatalf("failed to setup credentials for health-check: %s", err)
			}

			err = nats.Spawn(cfg)
			if err != nil {
				log.Fatalf("failed starting NATS: %+v", err)
			}
		},
	}

}
