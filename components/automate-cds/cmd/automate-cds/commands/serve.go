package commands

import (
	"fmt"
	"os"

	"github.com/spf13/cobra"
	"github.com/spf13/viper"

	"github.com/chef/automate/components/automate-cds/config"
	"github.com/chef/automate/components/automate-cds/server"
	"github.com/chef/automate/components/automate-cds/service"
	"github.com/chef/automate/lib/grpc/secureconn"
	"github.com/chef/automate/lib/logger"
)

var serveCmd = &cobra.Command{
	Use:   "serve",
	Short: "Launches the automate content delivery service proxy.",
	Run:   serve,
	Args:  cobra.ExactArgs(1),
}

func serve(cmd *cobra.Command, args []string) {
	cmd.PersistentFlags().StringP("log-level", "l", "info", "log level")
	cmd.PersistentFlags().StringP("log-format", "f", "text", "log format")
	cmd.PersistentFlags().String("grpc", "127.0.0.1:10154", "grpc host and port")

	viper.SetConfigFile(args[0])
	if err := viper.ReadInConfig(); err != nil {
		fail(fmt.Errorf("Could not read config file. Please pass a config file as the only argument to this command. %w", err))
	}

	cfg := config.AutomateCdsConfig{}
	if err := viper.Unmarshal(&cfg); err != nil {
		fail(fmt.Errorf("couldn't parse configuration file  %w", err))
	}

	l, err := logger.NewLogger(cfg.LogFormat, cfg.LogLevel)
	if err != nil {
		fail(fmt.Errorf("couldn't initialize logger %w", err))
	}

	cfg.FixupRelativeTLSPaths(args[0])
	serviceCerts, err := cfg.ReadCerts()
	if err != nil {
		fail(fmt.Errorf("Could not read certs %w", err))
	}
	connFactory := secureconn.NewFactory(*serviceCerts)

	service, err := service.Start(l, connFactory)
	if err != nil {
		fail(fmt.Errorf("could not initialize service %w", err))
	}

	fail(server.GRPC(cfg, service))
}

// fail outputs the error and exits with a non-zero code
func fail(err error) {
	// no error check: if this goes wrong, we're in trouble anyways
	fmt.Fprint(os.Stderr, err.Error()) // nolint: gas
	os.Exit(1)
}
