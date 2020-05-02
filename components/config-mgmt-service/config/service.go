//
//  Author:: Salim Afiune <afiune@chef.io>
//  Copyright:: Copyright 2017, Chef Software Inc.
//

package config

import (
	"fmt"
	"net/url"
	"path"

	log "github.com/sirupsen/logrus"
	"github.com/spf13/viper"

	"github.com/chef/automate/components/config-mgmt-service/backend"
	"github.com/chef/automate/components/config-mgmt-service/backend/elastic"
	"github.com/chef/automate/components/config-mgmt-service/backend/mock"
	platform_config "github.com/chef/automate/lib/platform/config"
	"github.com/chef/automate/lib/tls/certs"
	"github.com/chef/automate/lib/version"
)

var SERVICE_NAME = "config-mgmt-service"

// Service config definition
type Service struct {
	Name         string `json:"name" mapstructure:"name"`
	Version      string `json:"version" mapstructure:"version"`
	Host         string `json:"host" mapstructure:"host"`
	Port         int    `json:"port" mapstructure:"port"`
	LogLevel     string `json:"log_level" mapstructure:"log_level"`
	Postgres     `json:"postgres" mapstructure:"postgres"`
	client       backend.Client
	TLSConfig    certs.TLSConfig `mapstructure:"tls"`
	serviceCerts *certs.ServiceCerts
}

type Postgres struct {
	URI          string `mapstructure:"uri"`
	Database     string `mapstructure:"database"`
	SchemaPath   string `mapstructure:"schema_path"`
	MaxOpenConns int    `mapstructure:"max_open_conns"`
	MaxIdleConns int    `mapstructure:"max_idle_conns"`
}

// Default returns a blank Service instance with default parameters
func Default() *Service {
	return &Service{
		Version:  version.Version,
		Name:     SERVICE_NAME,
		Host:     "localhost",
		Port:     1234,
		LogLevel: "info",
	}
}

// New returns a Service instance with default parameters and backend client
func New(b backend.Client) *Service {
	service := Default()
	service.client = b
	return service
}

// ConfigFromViper returns a Service instance from the current viper config
func ConfigFromViper() (*Service, error) {
	cfg := Default()

	// Unmarshall the viper config into the server Config
	if err := viper.Unmarshal(cfg); err != nil {
		log.WithFields(log.Fields{
			"err": err,
		}).Error("Failed to marshall viper config to server config")
		return cfg, err
	}

	// Validates that the configuration has a valid host/port
	_, err := url.ParseRequestURI(path.Join("http://", cfg.ListenAddress()))
	if err != nil {
		log.WithFields(log.Fields{
			"err": err,
		}).Error(fmt.Sprintf("Listen adddress '%s' is not valid. Please check the 'host' and 'port' configuration", cfg.ListenAddress()))
		return cfg, err
	}

	// Setup the Elasticsearch backend
	var backend backend.Client
	selectedBackend := viper.GetString("backend")
	switch selectedBackend {
	case "elasticsearch":
		backend = elastic.New(viper.GetString("elasticsearch-url"))
	case "mock":
		backend = mock.New()
	default:
		err := fmt.Errorf("Invalid backend machanism %q set in configuration", selectedBackend)
		log.Error(err)
		return cfg, err
	}
	cfg.SetBackend(backend)

	// Fix any relative paths that might be in the config file before we
	// read in their values.
	cfg.TLSConfig.FixupRelativeTLSPaths(viper.ConfigFileUsed())
	serviceCerts, err := cfg.TLSConfig.ReadCerts()

	if err != nil {
		log.WithFields(log.Fields{
			"err": err.Error(),
		}).Error("Failed to loading x509 key pair and/or root CA certificate")
		return cfg, err
	}
	cfg.serviceCerts = serviceCerts

	if cfg.Postgres.URI == "" {
		var err error
		cfg.Postgres.URI, err = platform_config.PGURIFromEnvironment(cfg.Postgres.Database)
		if err != nil {
			log.WithError(err).Error("Failed to get pg uri")
			return nil, err
		}
	}

	// Log level
	cfg.SetLogLevel()

	return cfg, nil
}

func (s *Service) SetBackend(b backend.Client) {
	s.client = b
}

// Returns the configured backend
func (s *Service) GetBackend() backend.Client {
	return s.client
}

func (s *Service) GetServiceCerts() *certs.ServiceCerts {
	return s.serviceCerts
}

// ListenAddress is the address where gRPC server will bind and listen
func (c *Service) ListenAddress() string {
	return fmt.Sprintf("%s:%d", c.Host, c.Port)
}

// SetLogLevel sets the log level for the service
func (c *Service) SetLogLevel() {
	if c.LogLevel == "" {
		return
	}

	log.WithFields(log.Fields{
		"level": c.LogLevel,
	}).Info("Setting log level")

	level, err := log.ParseLevel(c.LogLevel)
	if err != nil {
		log.WithField("level", c.LogLevel).WithError(err).Error("Using default level 'info'")
		return
	}

	log.SetLevel(level)
}
