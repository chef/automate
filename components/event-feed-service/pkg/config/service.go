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
	DataLifecycle        `mapstructure:"data_lifecycle"`
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

// DataLifecycle number of days to keep and to remove the events after
type DataLifecycle struct {
	PurgeEventFeedAfterDays int `mapstructure:"purge_event_feed_after_days"`
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
