package commands

import (
	"fmt"
	"os"

	log "github.com/sirupsen/logrus"
	"github.com/spf13/cobra"
	"github.com/spf13/viper"
)

var cfgFile string

var RootCmd = &cobra.Command{
	Use:   "automate-gateway",
	Short: "Chef Automate API gRPC and REST gateway",
}

// Execute adds all child commands to the root command sets flags appropriately.
// This is called by main.main(). It only needs to happen once to the rootCmd.
func Execute() {
	if err := RootCmd.Execute(); err != nil {
		fmt.Println(err)
		os.Exit(-1)
	}
}

func init() {
	cobra.OnInitialize(initConfig)

	// global config
	RootCmd.PersistentFlags().StringVar(&cfgFile, "config", "", "config file (default is $HOME/.automate-gateway.yaml)")
}

// initConfig reads in config file and ENV variables if set.
func initConfig() {
	if cfgFile != "" {
		// enable ability to specify config file via flag
		viper.SetConfigFile(cfgFile)
	} else {
		// name of config file (without extension)
		viper.SetConfigName(".api-gateway")
		// adding home directory as first search path
		viper.AddConfigPath("$HOME")
	}

	viper.SetEnvPrefix("automate_gateway")
	// read in environment variables that match
	viper.AutomaticEnv()

	err := viper.ReadInConfig()
	if err != nil {
		log.WithFields(log.Fields{
			"file": viper.ConfigFileUsed(),
		}).Info("Error reading config file")
		return
	}
}
