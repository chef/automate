package config

import (
	"github.com/chef/automate/lib/tls/certs"

	log "github.com/sirupsen/logrus"
)

// EventFeed service specific config options
type EventFeed struct {
	Service              `mapstructure:"service"`
	*certs.TLSConfig     `mapstructure:"tls"`
	ElasticSearch        `mapstructure:"elasticsearch"`
	ElasticSearchSidecar `mapstructure:"elasticsearch_sidecar"`
	Cereal               `mapstructure:"cereal"`
	Jobs                 `mapstructure:"jobs"`
}

// Service is a base config options struct for all services
type Service struct {
	Host     string `mapstructure:"host"`
	Port     int    `mapstructure:"port"`
	LogLevel string `mapstructure:"log_level"`
}

// ElasticSearch configurations
type ElasticSearch struct {
	ElasticSearchURL string `mapstructure:"url"`
}

// ElasticSearchSidecar specific options
type ElasticSearchSidecar struct {
	ESSidecarAddress string `mapstructure:"address"`
}

// Cereal is the workflow backend for scheduled jobs
type Cereal struct {
	Address string `mapstructure:"address"`
}

// Jobs contains the default settings for the jobs scheduler. In many cases
// these are only applied when the job workflows are created, after which they
// are managed through gRPC requests and not config.
type Jobs struct {
	DefaultPurgeAfterDays int `mapstructure:"default_purge_after_days"`
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
