package main

import (
	"fmt"
	"os"

	reportmanager "github.com/chef/automate/components/report-manager-service"
	"github.com/chef/automate/components/report-manager-service/config"
	"github.com/chef/automate/lib/grpc/secureconn"
	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"
	"github.com/spf13/cobra"
	"github.com/spf13/viper"

	platform_config "github.com/chef/automate/lib/platform/config"
)

func main() {
	if err := rootCmd.Execute(); err != nil {
		fmt.Println(err.Error())
		os.Exit(1)
	}
}

var cfgFile string
var conf config.ReportManager

var rootCmd = &cobra.Command{
	Use: "report-manager-service",
}

var serveCmd = &cobra.Command{
	Use:  "serve",
	Args: cobra.NoArgs,
	RunE: serve,
}

func initConfig() error {
	if cfgFile != "" {
		viper.SetConfigFile(cfgFile)
		if err := viper.ReadInConfig(); err != nil {
			return err
		}
	}

	if err := viper.Unmarshal(&conf); err != nil {
		return err
	}

	switch conf.Log.Level {
	case "trace":
		logrus.SetLevel(logrus.TraceLevel)
	case "debug":
		logrus.SetLevel(logrus.DebugLevel)
	case "info":
		logrus.SetLevel(logrus.InfoLevel)
	case "warn":
		logrus.SetLevel(logrus.WarnLevel)
	case "error":
		logrus.Error(logrus.ErrorLevel)
	}

	if conf.Service.Host == "" {
		conf.Service.Host = "127.0.0.1"
	}
	if conf.Service.Port == 0 {
		conf.Service.Port = 10152
	}

	if conf.CerealConfig.Target == "" {
		conf.CerealConfig.Target = "127.0.0.1:10101"
	}

	if conf.Storage.URI == "" {
		var err error
		conf.Storage.URI, err = platform_config.PGURIFromEnvironment(conf.Storage.Database)
		if err != nil {
			logrus.WithError(err).Error("Failed to get pg uri")
			return err
		}
	}

	if conf.ObjStore.BucketName == "" {
		conf.ObjStore.BucketName = "default"
	}
	return nil
}

func init() {
	rootCmd.AddCommand(serveCmd)

	serveCmd.PersistentFlags().StringVar(&cfgFile, "config", "", "config file")
}

func serve(*cobra.Command, []string) error {

	if err := initConfig(); err != nil {
		return errors.Wrap(err, "failed to load config")
	}

	conf.FixupRelativeTLSPaths(cfgFile)
	serviceCerts, err := conf.ReadCerts()
	if err != nil {
		return errors.Wrap(err, "Could not read certs")
	}
	connFactory := secureconn.NewFactory(*serviceCerts)

	err = reportmanager.Serve(conf, connFactory)
	if err != nil {
		return errors.Wrap(err, "Unable to serve the backend")
	}
	return nil
}
