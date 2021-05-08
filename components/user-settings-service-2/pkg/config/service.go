package config

import (
	platform_config "github.com/chef/automate/lib/platform/config"
	"github.com/chef/automate/lib/tls/certs"

	"github.com/pkg/errors"
	log "github.com/sirupsen/logrus"
	"github.com/spf13/viper"
)

const DefaultLogLevel = "info"

type Notifications struct {
	Service   *Service            `mapstructure:"service" toml:"service"`
	Postgres  *Postgres           `mapstructure:"postgres" toml:"postgres"`
	Secrets   *Secrets            `mapstructure:"secrets" toml:"secrets"`
	TLSConfig *certs.TLSConfig    `mapstructure:"tls" toml:"tls"`
	Certs     *certs.ServiceCerts `mapstructure:"-" toml:"-"`
}

type Service struct {
	ExternalFQDN string `mapstructure:"external_fqdn" toml:"external_fqdn"`
	Host         string `mapstructure:"host" toml:"host"`
	Port         int    `mapstructure:"port" toml:"port"`
	LogLevel     string `mapstructure:"log_level" toml:"log_level"`
}

type Postgres struct {
	URI          string `mapstructure:"uri" toml:"uri"`
	Database     string `mapstructure:"database" toml:"database"`
	SchemaPath   string `mapstructure:"schema_path" toml:"schema_path"`
	MaxOpenConns int    `mapstructure:"max_open_conns" toml:"max_open_conns"`
	MaxIdleConns int    `mapstructure:"max_idle_conns" toml:"max_idle_conns"`
}

type Secrets struct {
	Host string `mapstructure:"host" toml:"host"`
	Port int    `mapstructure:"port" toml:"port"`
}

// FromFile loads the config from the given path, load referenced files (e.g.,
// certs), sets derived config and populates global state (e.g., log level,
// etc.)
func FromFile(path string) (*Notifications, error) {
	n, err := UnmarshalFromFile(path)
	if err != nil {
		return nil, err
	}
	if err := n.FixupPGURI(); err != nil {
		return nil, err
	}
	if err := n.SetLogLevel(); err != nil {
		return nil, err
	}
	if err := n.ReadCerts(); err != nil {
		return nil, err
	}

	return n, nil
}

func UnmarshalFromFile(path string) (*Notifications, error) {
	viper.SetConfigFile(path)

	err := viper.ReadInConfig()
	if err != nil {
		return nil, errors.Wrapf(err, "failed to read config file at path %q", path)
	}

	log.WithFields(log.Fields{"file": viper.ConfigFileUsed()}).Info("Using config file")

	n := &Notifications{}
	if err := viper.Unmarshal(n); err != nil {
		return nil, errors.Wrapf(err, "could not parse config file at path %q", path)
	}

	return n, nil
}

func (n *Notifications) FixupPGURI() error {
	if n.Postgres.URI != "" {
		return nil
	}
	pgURI, err := platform_config.PGURIFromEnvironment(n.Postgres.Database)
	if err != nil {
		return errors.Wrapf(err, "PGURIFromEnvironment failed for database %q", n.Postgres.Database)
	}
	n.Postgres.URI = pgURI
	return nil
}

func (n *Notifications) SetLogLevel() error {
	return n.Service.SetLogLevel()
}

func (n *Notifications) ReadCerts() error {
	certs, err := n.TLSConfig.ReadCerts()
	if err != nil {
		return err
	}
	n.Certs = certs
	return nil
}

func (s *Service) SetLogLevel() error {
	desiredLevelStr := s.LogLevel
	if desiredLevelStr == "" {
		desiredLevelStr = DefaultLogLevel
	}

	desiredLevel, err := log.ParseLevel(desiredLevelStr)
	if err != nil {
		return errors.Wrapf(err, "failed to parse log level string %q", desiredLevel)
	}

	log.SetLevel(desiredLevel)
	return nil
}
