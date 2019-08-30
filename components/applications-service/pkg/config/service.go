package config

import (
	"github.com/chef/automate/components/applications-service/pkg/ingester"
	"github.com/chef/automate/components/applications-service/pkg/storage"
	"github.com/chef/automate/lib/tls/certs"

	log "github.com/sirupsen/logrus"
)

var ServiceName = "applications-service"

// Applications service specific config options
type Applications struct {
	Service          `mapstructure:"service"`
	Events           `mapstructure:"events"`
	Postgres         `mapstructure:"postgres"`
	Jobs             `mapstructure:"jobs"`
	*certs.TLSConfig `mapstructure:"tls"`
	storageClient    storage.Client
	ingesterClient   ingester.Client
}

// Service is a base config options struct for all services
type Service struct {
	Host        string `mapstructure:"host"`
	Port        int    `mapstructure:"port"`
	MetricsPort int    `mapstructure:"metrics_port"`
	LogLevel    string `mapstructure:"log_level"`
	Enabled     bool   `mapstructure:"enabled"`
}

// Events holds configuration to connect to the NATS server inside the event-service
type Events struct {
	Host      string `mapstructure:"host"`
	Port      int    `mapstructure:"port"`
	ClusterID string `mapstructure:"cluster_id"`
}

type Postgres struct {
	URI        string `mapstructure:"uri"`
	Database   string `mapstructure:"database"`
	SchemaPath string `mapstructure:"schema_path"`
}

type Jobs struct {
	Host string `mapstructure:"host"`
	Port int    `mapstructure:"port"`
}

// SetIngester sets the ingester client for the service
func (s *Applications) SetIngester(i ingester.Client) {
	s.ingesterClient = i
}

// GetIngester returns the ingester client for the service
func (s *Applications) GetIngester() ingester.Client {
	return s.ingesterClient
}

// SetStorage sets the storage client for the service
func (s *Applications) SetStorage(c storage.Client) {
	s.storageClient = c
}

// GetStorage returns the storage client for the service
func (s *Applications) GetStorage() storage.Client {
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
