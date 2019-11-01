package commands

import (
	"fmt"
	"net/url"
	"os"

	"github.com/chef/automate/components/infra-proxy/server"
	"github.com/chef/automate/components/infra-proxy/service"
	"github.com/chef/automate/components/infra-proxy/storage/postgres/migration"

	platform_config "github.com/chef/automate/lib/platform/config"

	"github.com/chef/automate/lib/logger"
	"github.com/chef/automate/lib/tracing"
	"github.com/pkg/errors"
	"github.com/spf13/cobra"
	"github.com/spf13/viper"
)

var serveCmd = &cobra.Command{
	Use:   "serve",
	Short: fmt.Sprintf("Launches the automate infra proxy service..."),
	Run:   serve,
}

type config struct {
	GRPC           string `mapstructure:"grpc"`
	LogFormat      string `mapstructure:"log-format"`
	LogLevel       string `mapstructure:"log-level"`
	PGURL          string `mapstructure:"pg_url"`
	Database       string `mapstructure:"database"`
	MigrationsPath string `mapstructure:"migrations-path"`
}

func serve(cmd *cobra.Command, args []string) {

	cmd.PersistentFlags().StringP("log-level", "l", "info", "log level")
	cmd.PersistentFlags().StringP("log-format", "f", "text", "log format")
	cmd.PersistentFlags().String("grpc", "127.0.0.1:9093", "grpc host and port")
	cmd.PersistentFlags().StringP("pg_url", "p", "", "postgres uri")
	cmd.PersistentFlags().StringP("migrations-path", "m", "", "migrations path")

	viper.SetConfigFile(args[0])
	if err := viper.ReadInConfig(); err != nil {
		fail(errors.Wrap(err, `Could not read config file. Please pass a config file as the only argument to this command.`))
	}

	cfg := config{}
	if err := viper.Unmarshal(&cfg); err != nil {
		fail(errors.Wrap(err, "couldn't parse configuration file"))
	}

	if cfg.PGURL == "" {
		var err error
		cfg.PGURL, err = platform_config.PGURIFromEnvironment(cfg.Database)
		if err != nil {
			fail(errors.Wrap(err, "Failed to get pg uri"))
		}
	}

	closer, err := tracing.NewGlobalTracer("infra-proxy")
	if err == nil {
		defer tracing.CloseQuietly(closer)
	}

	l, err := logger.NewLogger(cfg.LogFormat, cfg.LogLevel)
	if err != nil {
		fail(errors.Wrap(err, "couldn't initialize logger"))
	}

	mustBeADirectory(cfg.MigrationsPath)
	u, err := url.Parse(cfg.PGURL)
	if err != nil {
		msg := fmt.Sprintf("could not parse pg_url %s from config", cfg.PGURL)
		fail(errors.Wrap(err, msg))
	}
	migrationConfig := migration.Config{
		Path:   cfg.MigrationsPath,
		PGURL:  u,
		Logger: l,
	}

	service, err := service.NewPostgresService(l, migrationConfig)
	if err != nil {
		fail(errors.Wrap(err, "could not initialize storage"))
	}

	fail(server.GRPC(cfg.GRPC, service))
}

// fail outputs the error and exits with a non-zero code
func fail(err error) {
	// no error check: if this goes wrong, we're in trouble anyways
	fmt.Fprint(os.Stderr, err.Error()) // nolint: gas
	os.Exit(1)
}

func mustBeADirectory(path string) {
	stat, err := os.Stat(path)
	if err == nil && stat.IsDir() {
		return // everything's in its right place
	} else if err != nil {
		fail(errors.Wrapf(err, "open path %#v", path))
	}
	fail(fmt.Errorf("path %#v is not a directory", path))
}
