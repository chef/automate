package commands

import (
	"github.com/pkg/errors"
	log "github.com/sirupsen/logrus"
	"github.com/spf13/cobra"

	"github.com/chef/automate/components/automate-gateway/gateway"
	"github.com/chef/automate/lib/tracing"
)

// serveCmd represents the serve command
var serveCmd = &cobra.Command{
	Use:   "serve",
	Short: "Launches the api webserver on https://localhost:2000",
	RunE: func(cmd *cobra.Command, args []string) error {
		log.Info("Starting Chef Automate Gateway Service")

		closer, err := tracing.NewGlobalTracer("automate-gateway")
		if err == nil {
			defer tracing.CloseQuietly(closer)
		}

		cfg, err := gateway.ConfigFromViper()
		if err != nil {
			return errors.Wrap(err, "loading configuration")
		}

		err = gateway.New(cfg).Start()
		log.WithError(err).Error("exiting")
		return err
	},
}

func init() {
	cobra.OnInitialize(initConfig)
	RootCmd.AddCommand(serveCmd)
}
