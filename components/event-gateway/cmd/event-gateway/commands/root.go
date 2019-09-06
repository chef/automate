package commands

import (
	log "github.com/sirupsen/logrus"
	"github.com/spf13/cobra"
	"github.com/spf13/viper"
)

var cfgFile string

// RootCmd is our application root command.
var RootCmd = &cobra.Command{
	Use:   "event-gateway",
	Short: "Chef Event Gateway",
}

func init() {
	cobra.OnInitialize(initConfig)

	// make config accessible from all commands
	RootCmd.PersistentFlags().StringVar(&cfgFile, "config", "", "path to the config file")

	RootCmd.AddCommand(newServeCmd())
	RootCmd.AddCommand(newHealthCheckCommand())
}

// initConfig reads in config file and ENV variables if set.
func initConfig() {
	// Note: health-check uses this function and we want it to produce no output
	// on success. So don't log non-errors here.

	// read in environment variables that match
	viper.SetEnvPrefix("event-gateway")
	viper.AutomaticEnv()

	if cfgFile != "" {
		viper.SetConfigFile(cfgFile)

		err := viper.ReadInConfig()
		if err != nil {
			log.WithFields(log.Fields{
				"error": err,
				"file":  viper.ConfigFileUsed(),
			}).Info("reading config file")
			return
		}
	}
}
