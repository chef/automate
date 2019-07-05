package config

import (
	"fmt"
	"net/url"
	"path"

	"github.com/chef/automate/lib/tls/certs"

	log "github.com/sirupsen/logrus"
	"github.com/spf13/viper"
)

const (
	COMPLIANCE_INGEST_KEY = "compliance_ingest"
	CFG_KEY               = "cfgingest"
	AUTHZ                 = "authz"
	EVENT_FEED            = "event_feed"
)

// Configuration for the Event Service
type EventConfig struct {
	ServiceConfig     ServiceConfig   `mapstructure:"service"`
	InternalMessaging Nats            `mapstructure:"internal_messaging"`
	Auth              Auth            `mapstructure:"auth"`
	StreamService     StreamService   `mapstructure:"stream_service"`
	LogConfig         LogConfig       `mapstructure:"log"`
	TLSConfig         certs.TLSConfig `mapstructure:"tls"`
	ServiceCerts      *certs.ServiceCerts
	HandlerEndpoints  HandlerConfig `mapstructure:"handlers"` // use to get an instance of a service's event handler
}

type Auth struct {
	AuthnEndpoint string `mapstructure:"authn_endpoint"`
	AuthzEndpoint string `mapstructure:"authz_endpoint"`
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
	Authz      string `mapstructure:"authz"`
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

	log.WithFields(log.Fields{
		"level": c.LogConfig.LogLevel,
	}).Info("Setting log level...")

	level, err := log.ParseLevel(c.LogConfig.LogLevel)
	if err != nil {
		log.WithField("level", c.LogConfig.LogLevel).WithError(err).Error("Using default level 'info'")
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
	log.Debug("config.go Configure() ->")
	config := &EventConfig{}

	// Unmarshal the viper config into the server Config
	if err := viper.Unmarshal(config); err != nil {
		log.WithFields(log.Fields{
			"err": err,
		}).Error("Failed to unmarshal config options to server config")
		return config, err
	}

	log.Debugf("Compliance handler endpoint: %s", config.HandlerEndpoints.Compliance)
	log.Debugf("Config ingest handler endpoint: %s", config.HandlerEndpoints.CfgIngest)
	log.Debugf("Authz handler endpoint: %s", config.HandlerEndpoints.Authz)
	log.Debugf("Event Feed handler endpoint: %s", config.HandlerEndpoints.EventFeed)

	// Validates that the configuration has a valid host/port
	_, err := url.ParseRequestURI(path.Join("http://", config.ListenAddress()))
	if err != nil {
		log.WithFields(log.Fields{
			"err": err,
		}).Error(fmt.Sprintf("Listen adddress '%s' is not valid. Please check the 'host' and 'port' configuration", config.ListenAddress()))
		return config, err
	}
	// Set log level
	config.SetLogLevel()

	// Fix any relative paths that might be in the config file
	config.TLSConfig.FixupRelativeTLSPaths(viper.ConfigFileUsed())
	serviceCerts, err := config.TLSConfig.ReadCerts()

	if err != nil {
		log.WithFields(log.Fields{
			"err": err.Error(),
		}).Error("Failed to load x509 key pair and/or root CA certificate")
		return config, err
	}
	config.ServiceCerts = serviceCerts
	log.Debugf("EVENT SERVICE CONFIG: %+v", config)

	log.Debug("end config.go Configure() ->")
	return config, nil
}
