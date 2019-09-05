package commands

import (
	log "github.com/sirupsen/logrus"
	"github.com/spf13/cobra"
	"github.com/spf13/viper"
)

var cfgFile string

// RootCmd is our application root command.
var RootCmd = &cobra.Command{
	Use:   "event-service",
	Short: "Chef Automate Event Service",
}

func init() {
	cobra.OnInitialize(initConfig)

	// make config accessible from all commands
	RootCmd.PersistentFlags().StringVar(&cfgFile, "config", "", "path to the config file")

	RootCmd.AddCommand(newServeCmd())
	RootCmd.AddCommand(newSendTestCmd())
}

// initConfig reads in config file and ENV variables if set.
func initConfig() {
	// read in environment variables that match
	viper.SetEnvPrefix("event-service")
	viper.AutomaticEnv()

	// set the config file if it's given
	if cfgFile != "" {
		viper.SetConfigFile(cfgFile)

		logctx := log.WithField("config_file", cfgFile)
		err := viper.ReadInConfig()
		if err != nil {
			logctx.WithError(err).Error("failed to read configuration")
			return
		}

		logctx.Info("Loaded configuration")
	}
}
