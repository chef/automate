package commands

import (
	"os"

	"github.com/sirupsen/logrus"
	"github.com/spf13/cobra"
	"github.com/spf13/viper"
)

var cfgFile string

// RootCmd is the command runner.
var RootCmd = &cobra.Command{
	Use:   "pg-sidecar-service",
	Short: "Chef Automate Postgres Sidecar",
}

// Execute adds all child commands to the root command sets flags appropriately.
// This is called by main.main(). It only needs to happen once to the rootCmd.
func Execute() {
	if err := RootCmd.Execute(); err != nil {
		logrus.Error(err)
		os.Exit(-1)
	}
}

func init() {
	cobra.OnInitialize(func() {
		if cfgFile != "" {
			viper.SetConfigFile(cfgFile)

			if err := viper.ReadInConfig(); err != nil {
				logrus.WithFields(logrus.Fields{
					"error": err,
					"file":  cfgFile,
				}).Error("failed to read config file")
			}
		}
	})

	RootCmd.PersistentFlags().StringVar(&cfgFile, "config", "", "path to a config file")
}
