package main

import (
	"fmt"
	"net/url"
	"os"

	"github.com/pkg/errors"
	"github.com/spf13/cobra"
	"github.com/spf13/viper"

	authz "github.com/chef/automate/api/interservice/authz/common"
	authz_v2 "github.com/chef/automate/api/interservice/authz/v2"
	"github.com/chef/automate/components/teams-service/server"
	"github.com/chef/automate/components/teams-service/service"
	"github.com/chef/automate/components/teams-service/storage/postgres/datamigration"
	"github.com/chef/automate/components/teams-service/storage/postgres/migration"
	"github.com/chef/automate/lib/grpc/secureconn"
	"github.com/chef/automate/lib/logger"
	platform_config "github.com/chef/automate/lib/platform/config"
	"github.com/chef/automate/lib/tls/certs"
	"github.com/chef/automate/lib/tracing"
	"github.com/chef/automate/lib/version"
)

func main() {
	cmd := &cobra.Command{
		Use:   "teams-service [config file]",
		Short: "teams-service is the teams management API for A2's Authorization",
		Args:  cobra.ExactArgs(1),
		Run:   serve,
	}

	// Note: we don't use global string variables, since all access to flags
	// goes through viper's unmarshalled config struct below
	cmd.PersistentFlags().StringP("log-level", "l", "info", "log level")
	cmd.PersistentFlags().StringP("log-format", "f", "text", "log format")
	cmd.PersistentFlags().String("grpc", "127.0.0.1:9093", "grpc host and port")
	cmd.PersistentFlags().StringP("pg_url", "p", "", "postgres uri")
	cmd.PersistentFlags().StringP("migrations-path", "m", "", "migrations path")
	cmd.PersistentFlags().StringP("data-migrations-path", "d", "", "data migrations path")
	cmd.PersistentFlags().StringP("authz-address", "a", "", "authz-service GRPC address")

	// bind all flags to viper config
	if err := viper.BindPFlags(cmd.PersistentFlags()); err != nil {
		fail(err)
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
	AuthzAddress       string `mapstructure:"authz-address"`
}

func serve(_ *cobra.Command, args []string) {
	viper.SetConfigFile(args[0])
	if err := viper.ReadInConfig(); err != nil {
		fail(errors.Wrap(err, `Could not read config file.
Please pass a config file as the only argument to this command.`))
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

	cfg.FixupRelativeTLSPaths(args[0])
	serviceCerts, err := cfg.ReadCerts()
	if err != nil {
		fail(errors.Wrap(err, "Could not read certs"))
	}

	closer, err := tracing.NewGlobalTracer("teams-service")
	if err == nil {
		defer tracing.CloseQuietly(closer)
	}

	connFactory := secureconn.NewFactory(*serviceCerts, secureconn.WithVersionInfo(
		version.Version,
		version.GitSHA,
	))

	l, err := logger.NewLogger(cfg.LogFormat, cfg.LogLevel)
	if err != nil {
		fail(errors.Wrap(err, "couldn't initialize logger"))
	}

	if cfg.AuthzAddress == "" {
		fail(errors.New("missing required config authz_address"))
	}
	authzConn, err := connFactory.Dial("authz-service", cfg.AuthzAddress)
	if err != nil {
		fail(errors.Wrapf(err, "failed to dial authz-service at (%s)", cfg.AuthzAddress))
	}
	authzClient := authz.NewSubjectPurgeClient(authzConn)
	authzV2Client := authz_v2.NewPoliciesClient(authzConn)

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

	service, err := service.NewPostgresService(l, connFactory,
		migrationConfig, dataMigrationConfig, authzClient, authzV2Client)
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
