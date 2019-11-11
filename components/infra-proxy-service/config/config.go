package config

import (
	platform_config "github.com/chef/automate/lib/platform/config"
	log "github.com/sirupsen/logrus"
	"github.com/spf13/viper"
)

//InfraProxyConfig configuration for the Infra Proxy Service
type InfraProxyConfig struct {
	LogConfig      LogConfig      `mapstructure:"log"`
	PostgresConfig PostgresConfig `mapstructure:"postgres"`
}

//LogConfig configuration for the Infra Proxy Service
type LogConfig struct {
	LogLevel  string `mapstructure:"log_level"`
	LogFormat string `mapstructure:"log_format"`
}

//PostgresConfig configuration for the Infra Proxy Service
type PostgresConfig struct {
	ConnectionString string `mapstructure:"uri"`
	Database         string `mapstructure:"database"`
	MigrationsPath   string `mapstructure:"migrations_path"`
}

// SetLogLevel sets the log level for the service
func (c *InfraProxyConfig) SetLogLevel() {
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

// Configure marshals configuration from the data-feed-service
// configuration file that has been read in by Viper during
// the root command's initConfig(). Settings are parsed by
// Viper into the InfraProxyConfig struct.
func Configure() (*InfraProxyConfig, error) {
	log.Debug("config.go Configure() ->")
	config := &InfraProxyConfig{}

	// Unmarshal the viper config into the server Config
	if err := viper.Unmarshal(config); err != nil {
		log.WithFields(log.Fields{
			"err": err,
		}).Error("Failed to unmarshal config options to server config")
		return config, err
	}

	// Set log level
	config.SetLogLevel()

	if config.PostgresConfig.ConnectionString == "" {
		var err error
		log.Infof("Database %s", config.PostgresConfig.Database)
		config.PostgresConfig.ConnectionString, err = platform_config.PGURIFromEnvironment(config.PostgresConfig.Database)
		log.Infof("Database connection string %s", config.PostgresConfig.ConnectionString)
		if err != nil {
			log.WithError(err).Error("Failed to get pg uri")
			return nil, err
		}
	}
	log.Debugf("INFRA PROXY SERVICE CONFIG: %+v", config)
	log.Debug("end config.go Configure() ->")
	return config, nil
}
