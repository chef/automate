package commands

import (
	"github.com/pkg/errors"
	log "github.com/sirupsen/logrus"
	"github.com/spf13/cobra"
	"github.com/spf13/viper"

	"github.com/chef/automate/components/event-feed-service/pkg/config"
	"github.com/chef/automate/components/event-feed-service/pkg/grpc"
	"github.com/chef/automate/lib/grpc/secureconn"
	"github.com/chef/automate/lib/tls/certs"
)

// Config defines the available configuration options
type Config struct {
	Host            string `mapstructure:"host"`
	Port            int    `mapstructure:"port"`
	certs.TLSConfig `mapstructure:"tls"`
}

var serveCmd = &cobra.Command{
	Use:   "serve",
	Short: "Launches Event Feed services",
	RunE: func(cmd *cobra.Command, args []string) error {
		conf, err := configFromViper()
		if err != nil {
			return errors.Wrap(err, "failed to load config")
		}

		conf.Service.SetLogLevel()

		tlsOpts, err := conf.ReadCerts()
		if err != nil {
			return errors.Wrap(err, "failed to read TLS certs")
		}

		connFactory := secureconn.NewFactory(*tlsOpts)
		log.Info("Starting Event Feed Service")

		return grpc.Spawn(conf, connFactory)
	},
}

func configFromViper() (*config.EventFeed, error) {
	cfg := &config.EventFeed{}
	if err := viper.Unmarshal(cfg); err != nil {
		log.WithFields(log.Fields{
			"error": err.Error(),
		}).Fatal("Failed to marshal config options to server config")
	}
	cfg.FixupRelativeTLSPaths(viper.ConfigFileUsed())

	return cfg, nil
}

func init() {
	RootCmd.AddCommand(serveCmd)
}
