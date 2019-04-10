package commands

import (
	"os"

	"github.com/chef/automate/components/config-mgmt-service/config"
	"github.com/sirupsen/logrus"
	"github.com/spf13/cobra"
	"github.com/spf13/viper"
)

var cfgFile string

var RootCmd = &cobra.Command{
	Use:   config.SERVICE_NAME,
	Short: "Config Management API Service",
}

// Execute adds all child commands to the root command sets flags appropriately.
// This is called by main.main(). It only needs to happen once to the rootCmd.
func Execute() {
	if err := RootCmd.Execute(); err != nil {
		logrus.Error(err)
		os.Exit(1)
	}
}

func init() {
	cobra.OnInitialize(initConfig)

	// global config
	RootCmd.PersistentFlags().StringVar(&cfgFile, "config", "", "config file (default is $HOME/.config-mgmt-service.yaml)")
}

// initConfig reads in config file and ENV variables if set.
func initConfig() {
	viper.SetConfigName(".config-mgmt-service") // name of config file (without extension)
	viper.AddConfigPath("$HOME")                // adding home directory as first search path
	// Set environment variable prefix, eg: AUTOMATE_CONFIG_MGMT_PORT
	viper.SetEnvPrefix("automate_config_mgmt")

	// Override the default config file if a config file has been passed as a flag
	if cfgFile != "" {
		viper.SetConfigFile(cfgFile)
	}

	// If a config file is found, read it in.
	if err := viper.ReadInConfig(); err == nil {
		logrus.Infof("Using config file: %s", viper.ConfigFileUsed())
	}

	// Override our config with any matching environment variables
	viper.AutomaticEnv()
}
