package config

import (
	"fmt"
	"net/url"
	"os"

	"github.com/pkg/errors"
	"github.com/spf13/viper"

	secrets "github.com/chef/automate/api/external/secrets"
	"github.com/chef/automate/api/interservice/authz"
	"github.com/chef/automate/components/infra-proxy-service/service"
	"github.com/chef/automate/components/infra-proxy-service/storage/postgres/migration"
	"github.com/chef/automate/lib/grpc/secureconn"
	"github.com/chef/automate/lib/logger"
	platform_config "github.com/chef/automate/lib/platform/config"
	"github.com/chef/automate/lib/tls/certs"
)

// Service config options
type Service struct {
	GRPC            string `mapstructure:"grpc"`
	LogFormat       string `mapstructure:"log-format"`
	LogLevel        string `mapstructure:"log-level"`
	certs.TLSConfig `mapstructure:"tls"`
	PGURL           string `mapstructure:"pg_url"`
	Database        string `mapstructure:"database"`
	MigrationsPath  string `mapstructure:"migrations-path"`
	AuthzAddress    string `mapstructure:"authz-address"`
	SecretsAddress  string `mapstructure:"secrets-address"`
}

// ConfigFromViper returns a Service instance from the current viper config
func ConfigFromViper(configFile string) (*service.Service, error) {
	// Set the file name of the configurations file
	viper.SetConfigName("config")
	// Set the configuration file type
	viper.SetConfigType("yaml")
	// Set the path to look for the configurations file
	viper.AddConfigPath("../dev")

	if err := viper.ReadInConfig(); err != nil {
		fail(errors.Wrap(err, `Could not read config file. Please pass a config file as the only argument to this command.`))
	}

	cfg := Service{}
	if err := viper.Unmarshal(&cfg); err != nil {
		fail(errors.Wrap(err, "couldn't parse configuration file"))
	}

	pgURL, err := platform_config.PGURIFromEnvironment(cfg.Database)
	if err != nil {
		fail(errors.Wrap(err, "Failed to get pg uri"))
	}
	cfg.PGURL = pgURL

	l, err := logger.NewLogger(cfg.LogFormat, cfg.LogLevel)
	if err != nil {
		fail(errors.Wrap(err, "couldn't initialize logger"))
	}

	cfg.FixupRelativeTLSPaths(configFile)
	serviceCerts, err := cfg.ReadCerts()
	if err != nil {
		fail(errors.Wrap(err, "Could not read certs"))
	}
	connFactory := secureconn.NewFactory(*serviceCerts)

	mustBeADirectory(cfg.MigrationsPath)
	u, err := url.Parse(cfg.PGURL)
	if err != nil {
		fail(errors.Wrapf(err, "could not parse pg_url %s from config", cfg.PGURL))
	}
	migrationConfig := migration.Config{
		Path:   cfg.MigrationsPath,
		PGURL:  u,
		Logger: l,
	}

	if cfg.AuthzAddress == "" {
		fail(errors.New("missing required config authz_address"))
	}
	authzConn, err := connFactory.Dial("authz-service", cfg.AuthzAddress)
	if err != nil {
		fail(errors.Wrapf(err, "failed to dial authz-service at (%s)", cfg.AuthzAddress))
	}
	authzClient := authz.NewAuthorizationServiceClient(authzConn)

	if cfg.SecretsAddress == "" {
		fail(errors.New("missing required config secrets_address"))
	}
	secretsConn, err := connFactory.Dial("secrets-service", cfg.SecretsAddress)
	if err != nil {
		fail(errors.Wrapf(err, "failed to dial secrets-service at (%s)", cfg.SecretsAddress))
	}

	// gets secrets client
	secretsClient := secrets.NewSecretsServiceClient(secretsConn)

	service, err := service.Start(l, migrationConfig, connFactory, secretsClient, authzClient)
	if err != nil {
		fail(errors.Wrap(err, "could not initialize storage"))
	}

	return service, nil
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
