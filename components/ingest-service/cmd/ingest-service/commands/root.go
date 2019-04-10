package commands

import (
	"os"

	log "github.com/sirupsen/logrus"
	"github.com/spf13/cobra"
	"github.com/spf13/viper"
)

var cfgFile string

var RootCmd = &cobra.Command{
	Use:   "ingest-service",
	Short: "Chef Automate Ingest Service",
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
	RootCmd.PersistentFlags().StringVar(&cfgFile, "config", "", "config file (default is $HOME/.ingest-service.toml)")
}

// initConfig reads in config file and ENV variables if set.
func initConfig() {
	if cfgFile != "" { // enable ability to specify where the config file via flag
		viper.SetConfigFile(cfgFile)
		// If a config file is found, read it in.
		if err := viper.ReadInConfig(); err != nil {
			log.WithFields(log.Fields{"warning": err}).Info("Given config file not found")
			log.WithFields(log.Fields{"path": cfgFile}).Info("Creating config file")
		}
	} else {
		viper.SetConfigName(".ingest-service") // name of config file (without extension)
		viper.AddConfigPath("$HOME")           // adding home directory as first search path
		viper.AddConfigPath(".")
		viper.AutomaticEnv() // read in environment variables that match

		// If a config file is found, read it in.
		if err := viper.ReadInConfig(); err == nil {
			log.WithFields(log.Fields{"file": viper.ConfigFileUsed()}).Info("Using config file")
		} else {
			log.WithFields(log.Fields{"warning": err}).Info("Config file not found")
			viper.SetConfigFile(".ingest-service.toml")
			log.Info("Creating a config file .ingest-service.toml in your local directory")
		}
	}

	log.WithFields(log.Fields{"path": viper.ConfigFileUsed()}).Info("Config loaded")
}
