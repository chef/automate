package commands

import (
	log "github.com/sirupsen/logrus"
	"github.com/spf13/cobra"
	"github.com/spf13/viper"

	"github.com/chef/automate/components/secrets-service/config"
	"github.com/chef/automate/components/secrets-service/grpc"
	"github.com/chef/automate/lib/grpc/secureconn"
	"github.com/chef/automate/lib/platform"
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
	Short: "Launches secrets services GRPC on port 10131 by default.",
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
			log.WithFields(log.Fields{
				"error": err.Error(),
			}).Fatal("Failed to load SSL key/cert files")
		}
		connFactory := secureconn.NewFactory(*tlsOpts)
		log.Info("Starting Secrets Services")

		grpc.Spawn(conf, connFactory)
	},
}

func configFromViper() (*config.Secrets, error) {
	cfg := &config.Secrets{}
	if err := viper.Unmarshal(cfg); err != nil {
		log.WithFields(log.Fields{
			"error": err.Error(),
		}).Fatal("Failed to marshal config options to server config")
	}
	cfg.FixupRelativeTLSPaths(viper.ConfigFileUsed())

	if cfg.Postgres.ConnectionString == "" {
		var err error
		cfg.Postgres.ConnectionString, err = platform.PGURIFromEnvironment(cfg.Database)
		if err != nil {
			log.WithError(err).Error("Failed to get pg uri")
			return nil, err
		}
	}

	return cfg, nil
}

func init() {
	RootCmd.AddCommand(serveCmd)
}
