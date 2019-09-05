package config

import (
	"fmt"
	"net/url"
	"path"

	"github.com/chef/automate/lib/tls/certs"
	"github.com/pkg/errors"

	log "github.com/sirupsen/logrus"
	"github.com/spf13/viper"
)

const (
	ComplianceEventName     = "compliance_ingest"
	ConfigMgmtEventName     = "cfgingest"
	EventFeedEventName      = "event_feed"
	ScanJobCreatedEventName = "scanJobCreated"
	ScanJobUpdatedEventName = "scanJobUpdated"
	ScanJobDeletedEventName = "scanJobDeleted"
	ProfileCreatedEventName = "profileCreated"
	ProfileUpdatedEventName = "profileUpdate"
	ProfileDeletedEventName = "profileDeleted"
	NodeTerminatedEventName = "nodeTerminated"
)

// Configuration for the Event Service
type EventConfig struct {
	ServiceConfig     ServiceConfig   `mapstructure:"service"`
	InternalMessaging Nats            `mapstructure:"internal_messaging"`
	StreamService     StreamService   `mapstructure:"stream_service"`
	LogConfig         LogConfig       `mapstructure:"log"`
	TLSConfig         certs.TLSConfig `mapstructure:"tls"`
	ServiceCerts      *certs.ServiceCerts
	HandlerEndpoints  HandlerConfig `mapstructure:"handlers"` // use to get an instance of a service's event handler
}

// Nats holds the configuration for the NATs Streaming Server
type Nats struct {
	Port        int `mapstructure:"port"`
	GatewayPort int `mapstructure:"gateway_port"`
}

type StreamService struct {
	Enabled   bool   `mapstructure:"enabled"`
	ClusterID string `mapstructure:"cluster_id"`
	Store     string `mapstructure:"store"`
}

type LogConfig struct {
	LogLevel  string `mapstructure:"log_level"`
	LogFormat string `mapstructure:"log_format"`
}

type ServiceConfig struct {
	Host          string `mapstructure:"host"`
	Port          uint16 `mapstructure:"port"`
	EventLimit    int    `mapstructure:"event_limit"`    // service's input channel size
	ListenerLimit int    `mapstructure:"listener_limit"` // number of concurrent listeners supported
}

type HandlerConfig struct {
	Compliance string `mapstructure:"feed"`
	CfgIngest  string `mapstructure:"cfgingest"`
	EventFeed  string `mapstructure:"event_feed"`
}

// ListenAddress is the address where gRPC server will bind and listen
func (c *EventConfig) ListenAddress() string {
	return fmt.Sprintf("%s:%d", c.ServiceConfig.Host, c.ServiceConfig.Port)
}

// SetLogLevel sets the log level for the service
func (c *EventConfig) SetLogLevel() {
	if c.LogConfig.LogLevel == "" {
		return
	}

	level, err := log.ParseLevel(c.LogConfig.LogLevel)
	if err != nil {
		log.WithField("level", c.LogConfig.LogLevel).WithError(err).Error("invalid log level, falling back to level 'info'")
		return
	}

	log.SetLevel(level)
}

func (c *EventConfig) GetCerts() *certs.ServiceCerts {
	return c.ServiceCerts
}

// Configure marshals configuration from the event-service
// configuration file that has been read in by Viper during
// the root command's initConfig(). Settings are parsed by
// Viper into the EventConfig struct.
func Configure() (*EventConfig, error) {
	config := &EventConfig{}

	// Unmarshal the viper config into the server Config
	if err := viper.Unmarshal(config); err != nil {
		return config, errors.Wrap(err, "failed to unmarshal config options to server config")
	}

	log.WithFields(log.Fields{
		"compliance": config.HandlerEndpoints.Compliance,
		"ingest":     config.HandlerEndpoints.CfgIngest,
		"event_feed": config.HandlerEndpoints.EventFeed,
	}).Debug("using endpoints")

	// Validates that the configuration has a valid host/port
	_, err := url.ParseRequestURI(path.Join("http://", config.ListenAddress()))
	if err != nil {
		return config, errors.Wrapf(err, "failed to parse listen address: %s", config.ListenAddress())
	}
	// Set log level
	config.SetLogLevel()

	// Fix any relative paths that might be in the config file
	config.TLSConfig.FixupRelativeTLSPaths(viper.ConfigFileUsed())
	serviceCerts, err := config.TLSConfig.ReadCerts()
	if err != nil {
		return config, errors.Wrap(err, "failed to load TLS certs")
	}

	config.ServiceCerts = serviceCerts
	log.WithField("config", config).Debug("event-service config")

	return config, nil
}
