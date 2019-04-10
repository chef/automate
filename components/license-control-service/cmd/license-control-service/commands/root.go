package commands

import (
	"os"

	log "github.com/sirupsen/logrus"
	"github.com/spf13/cobra"
	"github.com/spf13/viper"
)

var cfgFile string

// RootCmd is our application root command.
var RootCmd = &cobra.Command{
	Use:   "license-control-service",
	Short: "Chef Automate License Control Service",
}

// Execute adds all child commands to the root command sets flags appropriately.
// This is called by main.main(). It only needs to happen once to the rootCmd.
func Execute() {
	if err := RootCmd.Execute(); err != nil {
		log.Error(err)
		os.Exit(-1)
	}
}

func init() {
	cobra.OnInitialize(initConfig)

	// global config
	RootCmd.PersistentFlags().StringVar(&cfgFile, "config", "", "path to the config file")
}

// initConfig reads in config file and ENV variables if set.
func initConfig() {
	// read in environment variables that match
	viper.SetEnvPrefix("automate_license_control")
	viper.AutomaticEnv()

	// set the config file if it's given
	if cfgFile != "" {
		viper.SetConfigFile(cfgFile)

		err := viper.ReadInConfig()
		if err != nil {
			log.WithFields(log.Fields{
				"file": viper.ConfigFileUsed(),
			}).Info("Error reading config file")
			return
		}
	}
}
