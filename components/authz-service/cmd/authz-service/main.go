package main

import (
	"context"
	"fmt"
	"net/url"
	"os"

	"github.com/pkg/errors"
	"github.com/spf13/cobra"
	"github.com/spf13/viper"

	"github.com/chef/automate/components/authz-service/engine/opa"
	"github.com/chef/automate/components/authz-service/server"
	"github.com/chef/automate/components/authz-service/storage/postgres/datamigration"
	"github.com/chef/automate/components/authz-service/storage/postgres/migration"
	"github.com/chef/automate/lib/grpc/secureconn"
	"github.com/chef/automate/lib/logger"
	"github.com/chef/automate/lib/platform"
	"github.com/chef/automate/lib/tls/certs"
	"github.com/chef/automate/lib/tracing"
)

func main() {
	cmd := &cobra.Command{
		Use:   "authz-service <CONFIG_FILE>",
		Short: "Authz-service allows or denies API calls made through the gateway",
		Args:  cobra.ExactArgs(1),
		Run:   serve,
	}

	if err := cmd.Execute(); err != nil {
		fail(err)
	}
}

type config struct {
	GRPC               string `mapstructure:"grpc"`
	LogFormat          string `mapstructure:"log-format"`
	LogLevel           string `mapstructure:"log-level"`
	certs.TLSConfig    `mapstructure:"tls"`
	PGURL              string `mapstructure:"pg_url"`
	Database           string `mapstructure:"database"`
	MigrationsPath     string `mapstructure:"migrations-path"`
	DataMigrationsPath string `mapstructure:"data-migrations-path"`
	EventAddress       string `mapstructure:"event-address"`
}

func serve(_ *cobra.Command, args []string) {
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	viper.SetConfigFile(args[0])
	if err := viper.ReadInConfig(); err != nil {
		fail(errors.Wrap(err, `could not read config file.
Please pass a config file as the only argument to this command.`))
	}

	cfg := config{}
	if err := viper.Unmarshal(&cfg); err != nil {
		fail(errors.Wrap(err, "could not parse configuration file"))
	}

	if cfg.PGURL == "" {
		var err error
		cfg.PGURL, err = platform.PGURIFromEnvironment(cfg.Database)
		if err != nil {
			fail(errors.Wrap(err, "Failed to get pg uri"))
		}
	}

	cfg.FixupRelativeTLSPaths(args[0])
	serviceCerts, err := cfg.ReadCerts()
	if err != nil {
		fail(errors.Wrap(err, "could not read certs"))
	}

	connFactory := secureconn.NewFactory(*serviceCerts)

	l, err := logger.NewLogger(cfg.LogFormat, cfg.LogLevel)
	if err != nil {
		fail(errors.Wrap(err, "could not initialize logger"))
	}

	closer, err := tracing.NewGlobalTracer("authz-service")
	if err == nil {
		defer tracing.CloseQuietly(closer)
	}

	engine, err := opa.New(ctx, l)
	if err != nil {
		fail(errors.Wrap(err, "could not initialize engine"))
	}

	mustBeADirectory(cfg.MigrationsPath)
	mustBeADirectory(cfg.DataMigrationsPath)
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

	// NOTE (TC): These are IAM V2 specific data migrations.
	// Read more in v2_data_migrations.md.
	dataMigrationConfig := datamigration.Config{
		Path:   cfg.DataMigrationsPath,
		PGURL:  u,
		Logger: l,
	}

	// if server.GRPC() returns, it's with an error
	fail(server.GRPC(ctx, cfg.GRPC, l, connFactory, engine, migrationConfig,
		dataMigrationConfig, cfg.EventAddress))
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
		fail(errors.Wrapf(err, "open path %q", path))
	}
	fail(fmt.Errorf("path %q is not a directory", path))
}
