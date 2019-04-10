package commands

import (
	"os"

	log "github.com/sirupsen/logrus"
	"github.com/spf13/cobra"

	"github.com/chef/automate/components/event-gateway/pkg/config"
	"github.com/chef/automate/components/event-gateway/pkg/nats"
)

func newHealthCheckCommand() *cobra.Command {
	var debug bool

	cmd := &cobra.Command{
		Use:   "health-check",
		Short: "Check the health of the event-gateway (NATS) server",
		Run: func(cmd *cobra.Command, args []string) {
			cfg, err := config.Configure()
			if err != nil {
				log.WithError(err).Error("Failed to read event-gateway configuration")
			}

			if debug {
				cfg.LogConfig.LogLevel = "debug"
				cfg.SetLogLevel()
			}

			// Always return healthy if not enabled
			if !cfg.Service.Enabled {
				os.Exit(0)
			}

			err = nats.ConnectivityCheck(cfg)
			if err != nil {
				log.WithError(err).Error("health-check failed")
				os.Exit(1)
			}
			os.Exit(0)
		},
	}

	cmd.PersistentFlags().BoolVar(&debug, "debug", false, "enable debug logging")
	return cmd
}
