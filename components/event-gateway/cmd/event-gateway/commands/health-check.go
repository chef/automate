package commands

import (
	"github.com/pkg/errors"
	"github.com/spf13/cobra"

	"github.com/chef/automate/components/event-gateway/pkg/config"
	"github.com/chef/automate/components/event-gateway/pkg/nats"
)

func newHealthCheckCommand() *cobra.Command {
	var debug bool

	cmd := &cobra.Command{
		Use:          "health-check",
		Short:        "Check the health of the event-gateway (NATS) server",
		SilenceUsage: true,
		RunE: func(cmd *cobra.Command, args []string) error {
			cfg, err := config.Configure()
			if err != nil {
				return errors.Wrap(err, "failed to read event-gateway configuration")
			}

			if debug {
				cfg.LogConfig.LogLevel = "debug"
				cfg.SetLogLevel()
			}

			// Always return healthy if not enabled
			if !cfg.Service.Enabled {
				return nil
			}

			err = nats.ConnectivityCheck(cfg)
			if err != nil {
				return errors.Wrap(err, "failed connectivity check")
			}

			return nil
		},
	}

	cmd.PersistentFlags().BoolVar(&debug, "debug", false, "enable debug logging")
	return cmd
}
