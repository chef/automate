package config

import (
	"fmt"
	"net/url"
	"path"
	"time"

	"github.com/chef/automate/lib/tls/certs"

	log "github.com/sirupsen/logrus"
	"github.com/spf13/viper"
)

//const (
//	FEED_KEY = "feed"
//	CFG_KEY  = "cfgingest"
//)

// Configuration for the Data Feed Service
type DataFeedConfig struct {
	ServiceConfig       ServiceConfig       `mapstructure:"service"`
	LogConfig           LogConfig           `mapstructure:"log"`
	TLSConfig           certs.TLSConfig     `mapstructure:"tls"`
	NotificationsConfig NotificationsConfig `mapstructure:"notifications"`
	SecretsConfig       SecretsConfig       `mapstructure:"secrets"`
	CfgmgmtConfig       CfgmgmtConfig       `mapstructure:"cfgmgmt"`
	ComplianceConfig    ComplianceConfig    `mapstructure:"compliance"`
	ServiceCerts        *certs.ServiceCerts
}

type LogConfig struct {
	LogLevel  string `mapstructure:"log_level"`
	LogFormat string `mapstructure:"log_format"`
}

type ServiceConfig struct {
	Host            string        `mapstructure:"host"`
	Port            uint16        `mapstructure:"port"`
	FeedInterval    time.Duration `mapstructure:"feed_interval"`
	AssetPageSize   int32         `mapstructure:"asset_page_size"`
	ReportsPageSize int32         `mapstructure:"reports_page_size"`
}

type NotificationsConfig struct {
	Target string `mapstructure:"target"`
}

type SecretsConfig struct {
	Target string `mapstructure:"target"`
}

type CfgmgmtConfig struct {
	Target string `mapstructure:"target"`
}

type ComplianceConfig struct {
	Target string `mapstructure:"target"`
}

// ListenAddress is the address where gRPC server will bind and listen
func (c *DataFeedConfig) ListenAddress() string {
	return fmt.Sprintf("%s:%d", c.ServiceConfig.Host, c.ServiceConfig.Port)
}

// SetLogLevel sets the log level for the service
func (c *DataFeedConfig) SetLogLevel() {
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

func (c *DataFeedConfig) GetCerts() *certs.ServiceCerts {
	return c.ServiceCerts
}

// Configure marshals configuration from the data-feed-service
// configuration file that has been read in by Viper during
// the root command's initConfig(). Settings are parsed by
// Viper into the DataFeedConfig struct.
func Configure() (*DataFeedConfig, error) {
	log.Debug("config.go Configure() ->")
	config := &DataFeedConfig{}

	// Unmarshal the viper config into the server Config
	if err := viper.Unmarshal(config); err != nil {
		log.WithFields(log.Fields{
			"err": err,
		}).Error("Failed to unmarshal config options to server config")
		return config, err
	}

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
	log.Debugf("DATA FEED SERVICE CONFIG: %+v", config)
	log.Debug("end config.go Configure() ->")
	return config, nil
}
