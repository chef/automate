package commands

import (
	"os"

	log "github.com/sirupsen/logrus"
	"github.com/spf13/cobra"
	"github.com/spf13/viper"
)

var cfgFile string

// RootCmd is the command runner.
var RootCmd = &cobra.Command{
	Use:   "es-sidecar-service",
	Short: "Chef Automate Elasticsearch Sidecar",
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
	RootCmd.PersistentFlags().StringVar(&cfgFile, "config", "", "config file (default is $HOME/.es-sidecar-service.toml)")
	RootCmd.PersistentFlags().String("root-cert", "", "path to CA")
	RootCmd.PersistentFlags().String("key", "", "path to key")
	RootCmd.PersistentFlags().String("cert", "", "path to cert")

	err := viper.BindPFlag("cert", RootCmd.PersistentFlags().Lookup("cert"))
	if err != nil {
		panic("BindPFlags failed in init :(")
	}

	err = viper.BindPFlag("root-cert", RootCmd.PersistentFlags().Lookup("root-cert"))
	if err != nil {
		panic("BindPFlags failed in init :(")
	}

	err = viper.BindPFlag("key", RootCmd.PersistentFlags().Lookup("key"))
	if err != nil {
		panic("BindPFlags failed in init :(")
	}
}

// initConfig reads in config file and ENV variables if set.
func initConfig() {
	viper.SetConfigName(".es-sidecar-service") // name of config file (without extension)
	viper.AddConfigPath("$HOME")               // adding home directory as first search path
	viper.AddConfigPath(".")

	// override default config file if config is passed in via cli
	if cfgFile != "" {
		viper.SetConfigFile(cfgFile)
	}

	// If a config file is found, read it in.
	if err := viper.ReadInConfig(); err == nil {
		log.WithFields(log.Fields{"file": viper.ConfigFileUsed()}).Info("Using config file")
	} else {
		log.WithFields(log.Fields{"error": err}).Info("Config file not found")
		viper.SetConfigFile(".es-sidecar-service.toml")
		log.Info("Creating a config file .es-sidecar-service.toml in your local directory")
	}
}
