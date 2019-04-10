package config

import (
	log "github.com/sirupsen/logrus"

	"github.com/chef/automate/lib/tls/certs"
)

// Secrets service specific config options
type Secrets struct {
	SecretsKey      `mapstructure:"secretskey"`
	Service         `mapstructure:"service"`
	Postgres        `mapstructure:"postgres"`
	certs.TLSConfig `mapstructure:"tls"`
}

// SecretsKey options
type SecretsKey struct {
	Key  string `mapstructure:"key"`
	File string `mapstructure:"file"`
}

// Postgres specific options
type Postgres struct {
	ConnectionString string `mapstructure:"uri"`
	Database         string `mapstructure:"database"`
	MigrationsPath   string `mapstructure:"migrations_path"`
}

// Service is a base config options struct for all services
type Service struct {
	Host     string `mapstructure:"host"`
	Port     int    `mapstructure:"port"`
	LogLevel string `mapstructure:"log_level"`
}

// SetLogLevel sets the log level for the service
func (s *Service) SetLogLevel() {
	if s.LogLevel == "" {
		return
	}

	log.WithFields(log.Fields{
		"level": s.LogLevel,
	}).Info("Setting log level")

	level, err := log.ParseLevel(s.LogLevel)
	if err != nil {
		log.WithField("level", s.LogLevel).WithError(err).Error("Using default level 'info'")
		return
	}

	log.SetLevel(level)
}
