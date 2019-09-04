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

// Jobs contains the default settings for the jobs scheduler. This is only applied
// when the job workflows are created, after which they are managed through the
// Purge gRPC Configure request, not the config.
type Jobs struct {
	DefaultPurgeAfterDays int `mapstructure:"default_purge_after_days"`
}

// SetLogLevel sets the log level for the service
func (s *Service) SetLogLevel() {
	if s.LogLevel == "" {
		return
	}

	logctx := log.WithField("level", s.LogLevel)
	logctx.Info("Setting log level")

	level, err := log.ParseLevel(s.LogLevel)
	if err != nil {
		logctx.WithError(err).Warn("invalid log level, falling back to level 'info'")
		return
	}

	log.SetLevel(level)
}
