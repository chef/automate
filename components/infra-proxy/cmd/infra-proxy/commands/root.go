package commands

import (
	log "github.com/sirupsen/logrus"
	"github.com/spf13/cobra"
	"github.com/spf13/viper"
)

var cfgFile string

// RootCmd is our application root command.
var RootCmd = &cobra.Command{
	Use:   "infra-proxy",
	Short: "Chef Automate infra proxy service",
}

func init() {
	log.Debug("root.go init() ->")
	cobra.OnInitialize(initConfig)

	// make config accessible from all commands
	RootCmd.PersistentFlags().StringVar(&cfgFile, "config", "", "path to the config file")

	RootCmd.AddCommand(serveCmd)

	log.Debug("end root.go init() ->")
}

// initConfig reads in config file and ENV variables if set.
func initConfig() {
	log.Debug("root.go initConfig() ->")
	// read in environment variables that match
	viper.SetEnvPrefix("infra-proxy")
	viper.AutomaticEnv()

	// set the config file if it's given
	log.Infof("Config file is %s", cfgFile)
	if cfgFile != "" {
		viper.SetConfigFile(cfgFile)

		err := viper.ReadInConfig()
		if err != nil {
			log.WithFields(log.Fields{
				"file": viper.ConfigFileUsed(),
			}).Errorf("Error reading config file %s", err)
			return
		}
	}

	log.Debug("end root.go initConfig() ->")
}
