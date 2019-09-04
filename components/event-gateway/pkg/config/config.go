package config

import (
	"github.com/pkg/errors"
	log "github.com/sirupsen/logrus"
	"github.com/spf13/viper"

	"github.com/chef/automate/lib/tls/certs"
)

// Configuration for the Event Service
type EventGatewayConfig struct {
	Service              Nats            `mapstructure:"service"`
	InternalEventService NatsGateway     `mapstructure:"internal_event_service"`
	Auth                 Auth            `mapstructure:"auth"`
	LogConfig            LogConfig       `mapstructure:"log"`
	TLSConfig            certs.TLSConfig `mapstructure:"tls"`
	ServiceCerts         *certs.ServiceCerts
	FrontendTLS          []certs.TLSConfig `mapstructure:"frontend_tls"`
}

// Nats holds the configuration for the NATs Server
type Nats struct {
	Host                       string `mapstructure:"host"`
	Port                       int    `mapstructure:"port"`
	GatewayPort                int    `mapstructure:"gateway_port"`
	Enabled                    bool   `mapstructure:"enabled"`
	HealthCheckCredentialsFile string `mapstructure:"health_check_credentials_file"`
	DisableFrontendTLS         bool   `mapstructure:"disable_frontend_tls"`
}

type NatsGateway struct {
	Host        string `mapstructure:"host"`
	GatewayPort int    `mapstructure:"gateway_port"`
}

type Auth struct {
	AuthnEndpoint string `mapstructure:"authn_endpoint"`
	AuthzEndpoint string `mapstructure:"authz_endpoint"`
}

type LogConfig struct {
	LogLevel  string `mapstructure:"log_level"`
	LogFormat string `mapstructure:"log_format"`
}

// Configure marshals configuration from the event-service
// configuration file that has been read in by Viper during
// the root command's initConfig(). Settings are parsed by
// Viper into the EventConfig struct.
func Configure() (*EventGatewayConfig, error) {
	// Note: health-check uses this function and we want it to produce no output
	// on success. So don't log non-errors here.

	config := &EventGatewayConfig{}

	// Unmarshal the viper config into the server Config
	if err := viper.Unmarshal(config); err != nil {
		return config, errors.Wrap(err, "failed to unmarshal config options to server config")
	}

	// Set log level
	config.SetLogLevel()

	// Fix any relative paths that might be in the config file
	config.TLSConfig.FixupRelativeTLSPaths(viper.ConfigFileUsed())
	serviceCerts, err := config.TLSConfig.ReadCerts()
	if err != nil {
		return config, errors.Wrap(err, "failed to read TLS certs")
	}
	config.ServiceCerts = serviceCerts
	log.WithField("config", config).Debug("event-gateway config")
	return config, nil
}

// SetLogLevel sets the log level for the service
func (c *EventGatewayConfig) SetLogLevel() {
	if c.LogConfig.LogLevel == "" {
		return
	}

	level, err := log.ParseLevel(c.LogConfig.LogLevel)
	if err != nil {
		log.WithError(err).Warn("invalid log level, falling back to level 'info'")
		return
	}

	log.SetLevel(level)
}
