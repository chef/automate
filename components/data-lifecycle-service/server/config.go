package server

import (
	"fmt"
	"io/ioutil"

	toml "github.com/burntsushi/toml"
	"github.com/pkg/errors"

	"github.com/chef/automate/components/data-lifecycle-service/storage"
	"github.com/chef/automate/lib/tls/certs"
)

// Config describes the configuration for the Data Lifecycle Service
type Config struct {
	ListenAddress     string                           `toml:"listen_address"`
	Port              uint16                           `toml:"port"`
	LogLevel          string                           `toml:"log_level"`
	DailyRunAt        *ValidatedTimeOfDay              `toml:"daily_run_at"`
	ManagedServices   map[string]storage.ServiceConfig `toml:"managed"`
	DataLifeCycleInfo *DataLifeCycleConfig             `toml:"data-lifecycle"`
	certs.TLSConfig   `toml:"tls"`
}

// DataLifeCycleConfig describes the configuration needed to manage this
// services data
type DataLifeCycleConfig struct {
	// PurgeOlderThanDays describes how long we keep time series data. Anything
	// older than this is allowed to be regularly removed.
	PurgeOlderThanDays int32 `toml:"purge_older_than_days"`
}

// ConfigFromToml loads a configuration file from a toml file
func ConfigFromToml(configPath string) (Config, error) {
	serverConfig := Config{}

	configData, err := ioutil.ReadFile(configPath)
	if err != nil {
		return serverConfig, errors.Wrapf(err, "failed to read config file %s", configPath)
	}

	err = toml.Unmarshal(configData, &serverConfig)
	if err != nil {
		return serverConfig, errors.Wrap(err, "failed to parse config file")
	}

	serverConfig.ManagedServices["data-lifecycle-service"] = storage.ServiceConfig{
		Address: serverConfig.getAddressString(),
		Secure:  true,
	}

	if serverConfig.DailyRunAt == nil {
		serverConfig.DailyRunAt = DefaultTimeOfDay()
	}

	serverConfig.FixupRelativeTLSPaths(configPath)

	return serverConfig, nil
}

func (cfg *Config) getAddressString() string {
	return fmt.Sprintf("%s:%d", cfg.ListenAddress, cfg.Port)
}
