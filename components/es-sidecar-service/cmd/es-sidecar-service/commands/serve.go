package commands

import (
	"os"
	"time"

	"github.com/pkg/errors"
	log "github.com/sirupsen/logrus"
	"github.com/spf13/cobra"
	"github.com/spf13/viper"

	"github.com/chef/automate/components/es-sidecar-service/pkg/config"
	"github.com/chef/automate/components/es-sidecar-service/pkg/grpc"
	"github.com/chef/automate/components/es-sidecar-service/pkg/server"
)

var serveCmd = &cobra.Command{
	Use:   "serve",
	Short: "Launches ES Sidecar services GRPC on port 10390 by default.",
	Run: func(cmd *cobra.Command, args []string) {
		conf, err := configFromViper()
		if err != nil {
			log.WithFields(log.Fields{
				"error": err.Error(),
			}).Fatal("Failed to load config")
		}
		conf.SetLogLevel()
		log.Info("Starting ES Sidecar Services")

		watcher, err := server.NewWatcher(
			conf.WatcherConfig.CheckInterval,
			conf.WatcherConfig.WarningBytesFree,
			conf.WatcherConfig.CriticalBytesFree,
			conf.ElasticsearchURL,
		)
		if err != nil {
			log.WithError(err).Fatal("Failed to start ES Watcher")
		}

		go watcher.Watch()
		err = grpc.Spawn(conf)
		if err != nil {
			os.Exit(1)
		}
	},
}

func configFromViper() (*config.Service, error) {
	cfg := &config.Service{}
	if err := viper.Unmarshal(cfg); err != nil {
		return nil, errors.Wrap(err, "Failed to unmarshal config")
	}

	cfg.FixupRelativeTLSPaths(viper.ConfigFileUsed())
	return cfg, nil
}

func init() {
	RootCmd.AddCommand(serveCmd)
	viper.SetDefault("host", "localhost")
	viper.SetDefault("port", 10390)
	viper.SetDefault("elasticsearch_url", "http://localhost:9200")
	viper.SetDefault("watcher.interval", time.Minute)
	viper.SetDefault("watcher.warning_bytes", 1<<30)  // Warn at 1GB
	viper.SetDefault("watcher.critical_bytes", 1<<29) // Warn at 500MB
}
