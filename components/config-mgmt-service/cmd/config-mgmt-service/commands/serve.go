package commands

import (
	"fmt"

	log "github.com/sirupsen/logrus"
	"github.com/spf13/cobra"

	"github.com/chef/automate/components/config-mgmt-service/config"
	"github.com/chef/automate/components/config-mgmt-service/server"
	"github.com/chef/automate/lib/tracing"
)

var serveCmd = &cobra.Command{
	Use:   "serve",
	Short: fmt.Sprintf("Launches the config-mgmt service on https://%s:%d", config.Default().Host, config.Default().Port),
	Run: func(cmd *cobra.Command, args []string) {
		closer, err := tracing.NewGlobalTracer("config-mgmt-service")
		if err == nil {
			defer tracing.CloseQuietly(closer)
		}

		conf, err := config.ConfigFromViper()
		if err != nil {
			log.WithFields(log.Fields{
				"error": err,
			}).Fatal("Failed to configure config-mgmt service")
		}

		log.Info("Starting config mgmt API service")

		// Start the server
		err = server.StartGRPC(conf)
		if err != nil {
			log.WithFields(log.Fields{"error": err}).Fatal("GRPC server failed to start")
		}
	},
}

func init() {
	RootCmd.AddCommand(serveCmd)
}
