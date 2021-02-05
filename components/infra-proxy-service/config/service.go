package config

import (
	"github.com/chef/automate/lib/tls/certs"
	log "github.com/sirupsen/logrus"
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

// Default returns a blank Service instance with default parameters
func Default() *Service {
	return &Service{
		GRPC: "127.0.0.1:10153",
        LogLevel: "info",
        MigrationsPath: "/src/components/infra-proxy-service/storage/postgres/schema/sql/",
	}
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
