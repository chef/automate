package commands

import (
	"github.com/chef/automate/components/automate-ha-pgleaderchk/pkg/pgleaderchk"
	_ "github.com/lib/pq"
	log "github.com/sirupsen/logrus"
	"github.com/spf13/cobra"
)

var serveCmd = &cobra.Command{
	Use:   "serve",
	Short: "Launches the webserver and checks local PostgreSQL for leader status",
	Run: func(cmd *cobra.Command, args []string) {
		log.Info("Starting Chef Automate Backend PostgreSQL Leader Check Service")

		cfg, err := pgleaderchk.ConfigFromViper()
		if err != nil {
			log.WithError(err).Fatal("Failed to configure service")
		}

		server, err := pgleaderchk.NewFromConfig(cfg)
		if err != nil {
			log.WithError(err).Fatal("Failed to configure service")
		}

		// if Serve returns, something went wrong
		if err := server.Serve(); err != nil {
			log.WithError(err).Fatal("Failed to start service")
		}
	},
}

func init() {
	RootCmd.AddCommand(serveCmd)
}
