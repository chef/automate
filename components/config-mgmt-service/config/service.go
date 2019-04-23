//
//  Author:: Salim Afiune <afiune@chef.io>
//  Copyright:: Copyright 2017, Chef Software Inc.
//

package config

import (
	"errors"
	"fmt"
	"net/url"
	"path"

	log "github.com/sirupsen/logrus"
	"github.com/spf13/viper"

	"github.com/chef/automate/components/config-mgmt-service/backend"
	"github.com/chef/automate/components/config-mgmt-service/backend/elastic"
	"github.com/chef/automate/components/config-mgmt-service/backend/mock"
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
	client       backend.Client
	TLSConfig    certs.TLSConfig `mapstructure:"tls"`
	serviceCerts *certs.ServiceCerts
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
	switch viper.GetString("backend") {
	case "elasticsearch":
		backend = elastic.New(viper.GetString("elasticsearch-url"))
	case "mock":
		backend = mock.New()
	default:
		err := errors.New("Unavailable backend machanism")
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
