package commands

import (
	log "github.com/sirupsen/logrus"
	"github.com/spf13/cobra"

	"github.com/chef/automate/components/automate-gateway/gateway"
	"github.com/chef/automate/lib/tracing"
)

// serveCmd represents the serve command
var serveCmd = &cobra.Command{
	Use:   "serve",
	Short: "Launches the api webserver on https://localhost:2000",
	Run: func(cmd *cobra.Command, args []string) {
		log.Info("Starting Chef Automate Gateway Service")

		closer, err := tracing.NewGlobalTracer("automate-gateway")
		if err == nil {
			defer tracing.CloseQuietly(closer)
		}

		cfg, err := gateway.ConfigFromViper()
		if err != nil {
			log.WithError(err).Fatal("Failed to configure service")
		}

		gw, err := gateway.NewFromConfig(cfg)
		if err != nil {
			log.WithError(err).Fatal("Failed to configure service")
		}

		// if Serve returns, something went wrong
		if err := gw.Serve(); err != nil {
			log.WithError(err).Fatal("Failed to start service")
		}
	},
}

func init() {
	cobra.OnInitialize(initConfig)
	RootCmd.AddCommand(serveCmd)
}
