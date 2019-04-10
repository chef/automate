package config

import (
	"time"

	log "github.com/sirupsen/logrus"

	es "github.com/chef/automate/components/es-sidecar-service/pkg/elastic"
	"github.com/chef/automate/lib/tls/certs"
)

type watcherConfig struct {
	CheckInterval     time.Duration `mapstructure:"interval"`
	WarningBytesFree  int64         `mapstructure:"warning_bytes"`
	CriticalBytesFree int64         `mapstructure:"critical_bytes"`
}

// Service defines the available configuration options for this service
type Service struct {
	Host             string        `mapstructure:"host"`
	Port             int           `mapstructure:"port"`
	LogLevel         string        `mapstructure:"log_level"`
	ElasticsearchURL string        `mapstructure:"elasticsearch_url"`
	WatcherConfig    watcherConfig `mapstructure:"watcher"`
	certs.TLSConfig  `mapstructure:"tls"`
	BackupsConfig    es.BackupsConfig `mapstructure:"backups"`
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
