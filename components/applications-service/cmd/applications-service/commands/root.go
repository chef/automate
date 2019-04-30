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
	Use:   "applications-service",
	Short: "Chef Automate Applications Service",
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
	RootCmd.PersistentFlags().StringVar(&cfgFile, "config", "", "config file (default is $HOME/.applications-service.toml)")
	RootCmd.PersistentFlags().String("root-cert", "", "path to CA")
	RootCmd.PersistentFlags().String("key", "", "path to key")
	RootCmd.PersistentFlags().String("cert", "", "path to cert")

	// https://github.com/spf13/viper/blob/7a605a50e69cd1ea431c4a1e6eebe9b287ef6de4/viper.go#L939
	// err is non-nil when the flag doesn't exist. panic here should get caught quickly by Ci.
	err := viper.BindPFlag("cert", RootCmd.PersistentFlags().Lookup("cert"))
	if err != nil {
		panic("viper configuration does not match command line flags")
	}
	err = viper.BindPFlag("root-cert", RootCmd.PersistentFlags().Lookup("root-cert"))
	if err != nil {
		panic("viper configuration does not match command line flags")
	}
	err = viper.BindPFlag("key", RootCmd.PersistentFlags().Lookup("key"))
	if err != nil {
		panic("viper configuration does not match command line flags")
	}
}

// initConfig reads in config file and ENV variables if set.
func initConfig() {
	viper.SetConfigName(".applications-service") // name of config file (without extension)
	viper.AddConfigPath("$HOME")                 // adding home directory as first search path
	viper.AddConfigPath(".")

	// override default config file if config is passed in via cli
	if cfgFile != "" {
		viper.SetConfigFile(cfgFile)
	}

	// If a config file is found, read it in.
	if err := viper.ReadInConfig(); err == nil {
		log.WithFields(log.Fields{"file": viper.ConfigFileUsed()}).Info("Using config file")
	}

	// Override our config with any matching environment variables
	viper.AutomaticEnv()
}
