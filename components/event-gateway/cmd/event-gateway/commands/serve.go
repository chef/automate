package commands

import (
	"fmt"

	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"
	"github.com/spf13/cobra"

	"github.com/chef/automate/components/event-gateway/pkg/config"
	"github.com/chef/automate/components/event-gateway/pkg/nats"
	"github.com/chef/automate/lib/tracing"
)

func newServeCmd() *cobra.Command {
	return &cobra.Command{
		Use:   "serve",
		Short: "Start the event-gateway (NATS) server",
		RunE: func(cmd *cobra.Command, args []string) error {
			logctx := logrus.WithField("config_file", cfgFile)

			cfg, err := config.Configure()
			if err != nil {
				return errors.Wrap(err, "configuring event-gateway")
			}

			uri := fmt.Sprintf("nats://%s:%d", cfg.Service.Host, cfg.Service.Port)
			logctx = logctx.WithFields(logrus.Fields{
				"log_level": cfg.LogConfig.LogLevel,
				"uri":       uri,
			})

			logctx.Info("Starting event-gateway")

			closer, err := tracing.NewGlobalTracer("event-gateway")
			if err != nil {
				return errors.Wrap(err, "starting tracer for event-gateway")
			}
			if closer != nil {
				defer tracing.CloseQuietly(closer)
			}

			err = nats.GenerateHealthCheckCredentials(cfg)
			if err != nil {
				return errors.Wrap(err, "setting up health-check credentials")
			}

			return nats.Spawn(cfg)
		},
	}
}
