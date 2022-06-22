package commands

import (
	log "github.com/sirupsen/logrus"
	"github.com/spf13/cobra"
	"github.com/spf13/viper"

	"github.com/chef/automate/components/user-settings-service/pkg/config"
	"github.com/chef/automate/components/user-settings-service/pkg/grpc"
	"github.com/chef/automate/components/user-settings-service/pkg/storage/postgres"
	"github.com/chef/automate/lib/grpc/secureconn"
	platform_config "github.com/chef/automate/lib/platform/config"
	"github.com/chef/automate/lib/version"

	"fmt"
)

var serveCmd = &cobra.Command{
	Use:   "serve",
	Short: "Launches applications services",
	RunE: func(cmd *cobra.Command, args []string) error {
		conf, err := configFromViper()
		if err != nil {
			log.WithFields(log.Fields{
				"error": err.Error(),
			}).Fatal("Failed to load config")
		}
		conf.Service.SetLogLevel()
		svcCerts, err := conf.ReadCerts()
		if err != nil {
			log.WithFields(log.Fields{
				"error": err.Error(),
			}).Fatal("Failed to load SSL key/cert files")
		}
		connFactory := secureconn.NewFactory(*svcCerts,
			secureconn.WithVersionInfo(version.Version, version.GitSHA))

		// Storage client (Postgres)
		fmt.Printf("US SS:: %+v", conf.Postgres)
		dbClient, err := postgres.ConnectAndMigrate(&conf.Postgres)
		if err != nil {
			log.WithFields(log.Fields{
				"error": err.Error(),
			}).Fatal("Failed to create postgres client")
		}
		conf.SetStorage(dbClient)

		// GRPC Server
		return grpc.Spawn(conf, connFactory)
	},
}

func configFromViper() (*config.UserSettings, error) {
	cfg := &config.UserSettings{}
	if err := viper.Unmarshal(cfg); err != nil {
		log.WithFields(log.Fields{
			"error": err.Error(),
		}).Fatal("Failed to marshal config options to server config")
	}
	cfg.FixupRelativeTLSPaths(viper.ConfigFileUsed())

	if cfg.Postgres.URI == "" {
		var err error
		cfg.Postgres.URI, err = platform_config.PGURIFromEnvironment(cfg.Postgres.Database)
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
