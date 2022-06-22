package commands

import (
	"fmt"
	log "github.com/sirupsen/logrus"
	"github.com/spf13/cobra"
	"github.com/spf13/viper"
	"os"
)

var cfgFile string

var RootCmd = &cobra.Command{
	Use:   "pgleaderchk",
	Short: "Chef Automate Backend PostgreSQL leader healthcheck http server",
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
	RootCmd.PersistentFlags().StringVarP(&cfgFile, "config", "c", "config.toml", "config file (default is ./config.toml)")
}

// initConfig reads in config file and ENV variables if set.
func initConfig() {
	if cfgFile != "" {
		// enable ability to specify config file via flag
		viper.SetConfigFile(cfgFile)
	} else {
		// name of config file (without extension)
		viper.SetConfigName("config.toml")
		// adding current directory as first search path
		viper.AddConfigPath(".")
	}

	viper.SetEnvPrefix("backend_pgleaderchk")
	// read in environment variables that match
	viper.AutomaticEnv()

	err := viper.ReadInConfig()
	if err != nil {
		log.WithFields(log.Fields{
			"file": viper.ConfigFileUsed(),
		}).Fatal("Error reading config file")
	}
}
