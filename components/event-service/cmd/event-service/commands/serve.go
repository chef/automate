package commands

import (
	"context"

	"fmt"

	"github.com/chef/automate/components/event-service/config"
	"github.com/chef/automate/components/event-service/nats"
	"github.com/chef/automate/components/event-service/server"
	"github.com/chef/automate/lib/tracing"
	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"
	"github.com/spf13/cobra"
)

func newServeCmd() *cobra.Command {
	return &cobra.Command{
		Use:   "serve",
		Short: "Start the event gRPC server",
		RunE: func(cmd *cobra.Command, args []string) error {
			logctx := logrus.WithField("config_file", cfgFile)

			cfg, err := config.Configure()
			if err != nil {
				return errors.Wrap(err, "configuring event-service")
			}

			uri := fmt.Sprintf("%s:%d", cfg.ServiceConfig.Host, cfg.ServiceConfig.Port)
			logctx = logctx.WithFields(logrus.Fields{
				"log_level": cfg.LogConfig.LogLevel,
				"uri":       uri,
			})

			logctx.Info("Starting event-service")

			closer, err := tracing.NewGlobalTracer("event-service")
			if err != nil {
				errors.Wrap(err, "starting tracer for event-service")
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
						logrus.WithError(err).Error("starting NATS")
					}
				}()
			}

			return server.StartGRPC(context.Background(), cfg)
		},
	}
}
