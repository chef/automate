package commands

import (
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
	Run: func(cmd *cobra.Command, args []string) {
		conf, err := configFromViper()
		if err != nil {
			log.WithFields(log.Fields{
				"error": err.Error(),
			}).Fatal("Failed to load config")
		}
		conf.Service.SetLogLevel()
		tlsOpts, err := conf.ReadCerts()
		if err != nil {
			log.WithError(err).Fatal("Failed to load SSL key/cert files")
		}
		connFactory := secureconn.NewFactory(*tlsOpts)
		log.Info("Starting Event Feed Service")

		err = grpc.Spawn(conf, connFactory)
		if err != nil {
			log.WithError(err).Fatal("gRPC server failed")
		}
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
