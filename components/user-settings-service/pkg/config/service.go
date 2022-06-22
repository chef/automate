package config

import (
	"github.com/chef/automate/components/user-settings-service/pkg/storage"
	"github.com/chef/automate/lib/tls/certs"

	log "github.com/sirupsen/logrus"
)

var ServiceName = "user-settings-service"

type UserSettings struct {
	Service          `mapstructure:"service"`
	Postgres         `mapstructure:"postgres"`
	*certs.TLSConfig `mapstructure:"tls"`
	storageClient    storage.Client
}

// Service is a base config options struct for all services
type Service struct {
	Host        string `mapstructure:"host"`
	Port        int    `mapstructure:"port"`
	MetricsPort int    `mapstructure:"metrics_port"`
	LogLevel    string `mapstructure:"log_level"`
}

type Postgres struct {
	URI          string `mapstructure:"uri"`
	Database     string `mapstructure:"database"`
	SchemaPath   string `mapstructure:"schema_path"`
	MaxOpenConns int    `mapstructure:"max_open_conns"`
	MaxIdleConns int    `mapstructure:"max_idle_conns"`
}

// SetStorage sets the storage client for the service
func (s *UserSettings) SetStorage(c storage.Client) {
	s.storageClient = c
}

// GetStorage returns the storage client for the service
func (s *UserSettings) GetStorage() storage.Client {
	return s.storageClient
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
