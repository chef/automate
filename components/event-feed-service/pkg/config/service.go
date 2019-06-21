package config

import (
	"github.com/chef/automate/lib/tls/certs"

	log "github.com/sirupsen/logrus"
)

// Event Feed service specific config options
type EventFeed struct {
	Service          `mapstructure:"service"`
	*certs.TLSConfig `mapstructure:"tls"`
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
