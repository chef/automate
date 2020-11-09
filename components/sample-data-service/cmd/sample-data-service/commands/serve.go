package commands

import (
	"fmt"
	"os"

	"github.com/pkg/errors"
	"github.com/spf13/cobra"
	"github.com/spf13/viper"

	"github.com/chef/automate/components/sample-data-service/server"
	"github.com/chef/automate/components/sample-data-service/service"

	"github.com/chef/automate/lib/grpc/secureconn"
	"github.com/chef/automate/lib/logger"
	"github.com/chef/automate/lib/tls/certs"
)

var serveCmd = &cobra.Command{
	Use:   "serve",
	Short: "Launches the automate sample data service.",
	Run:   serve,
}

type config struct {
	GRPC            string `mapstructure:"grpc"`
	LogFormat       string `mapstructure:"log-format"`
	LogLevel        string `mapstructure:"log-level"`
	certs.TLSConfig `mapstructure:"tls"`
}

func serve(cmd *cobra.Command, args []string) {
	cmd.PersistentFlags().StringP("log-level", "l", "info", "log level")
	cmd.PersistentFlags().StringP("log-format", "f", "text", "log format")
	cmd.PersistentFlags().String("grpc", "127.0.0.1:10155", "grpc host and port")

	viper.SetConfigFile(args[0])
	if err := viper.ReadInConfig(); err != nil {
		fail(errors.Wrap(err, `Could not read config file. Please pass a config file as the only argument to this command.`))
	}

	cfg := config{}
	if err := viper.Unmarshal(&cfg); err != nil {
		fail(errors.Wrap(err, "couldn't parse configuration file"))
	}

	l, err := logger.NewLogger(cfg.LogFormat, cfg.LogLevel)
	if err != nil {
		fail(errors.Wrap(err, "couldn't initialize logger"))
	}

	cfg.FixupRelativeTLSPaths(args[0])
	serviceCerts, err := cfg.ReadCerts()
	if err != nil {
		fail(errors.Wrap(err, "Could not read certs"))
	}
	connFactory := secureconn.NewFactory(*serviceCerts)

	service, err := service.Start(l, connFactory)
	if err != nil {
		fail(errors.Wrap(err, "could not initialize service"))
	}

	fail(server.GRPC(cfg.GRPC, service))
}

// fail outputs the error and exits with a non-zero code
func fail(err error) {
	// no error check: if this goes wrong, we're in trouble anyways
	fmt.Fprint(os.Stderr, err.Error()) // nolint: gas
	os.Exit(1)
}
