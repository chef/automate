package commands

import (
	"context"
	"os"

	"github.com/pkg/errors"
	log "github.com/sirupsen/logrus"
	"github.com/spf13/cobra"
	"github.com/spf13/viper"

	"github.com/chef/automate/components/pg-sidecar-service/pkg/server"
)

var serveCmd = &cobra.Command{
	Use:   "serve",
	Short: "Launches PG Sidecar services GRPC on port 10100 by default.",
	Run: func(cmd *cobra.Command, args []string) {
		conf, err := configFromViper()
		if err != nil {
			log.WithFields(log.Fields{
				"error": err.Error(),
			}).Fatal("Failed to load config")
		}

		/* TODO(yzl): We removed the function SetLogLevel() because we didn't think it was
		* doing anything. Do we want to put it back?
		conf.SetLogLevel()
		*/

		if err = server.StartGRPC(context.Background(), conf); err != nil {
			os.Exit(1)
		}
	},
}

func configFromViper() (*server.Config, error) {
	cfg := &server.Config{}
	if err := viper.Unmarshal(cfg); err != nil {
		return nil, errors.Wrap(err, "Failed to unmarshal config")
	}

	cfg.FixupRelativeTLSPaths(viper.ConfigFileUsed())
	return cfg, nil
}

func init() {
	RootCmd.AddCommand(serveCmd)
	viper.SetDefault("host", "localhost")
	viper.SetDefault("port", 10100)
}
