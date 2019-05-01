package config

import (
	"io/ioutil"
	"os"

	base_config "github.com/chef/automate/lib/config"
	toml "github.com/pelletier/go-toml"
	log "github.com/sirupsen/logrus"
)

// ProjectUpdateConfig - the config for project updating
type ProjectUpdateConfig struct {
	State           string   `toml:"state"`
	ProjectUpdateID string   `toml:"project_update_id"`
	EsJobIDs        []string `toml:"es_job_ids"`
}

func defaultConfig() aggregateConfig {
	return aggregateConfig{
		ProjectUpdateConfig: ProjectUpdateConfig{},
	}
}

// ConfigManager - configuration manager for the service
type ConfigManager struct {
	baseConfigManager *base_config.Manager
}

// Config - stores the configuration for the service
type aggregateConfig struct {
	ProjectUpdateConfig ProjectUpdateConfig `toml:"project_update_config"`
}

// NewConfigManager - create a new config. There should only be one config for the service.
func NewConfigManager(configFile string) *ConfigManager {
	config := readConfigFromFile(configFile, defaultConfig())
	return &ConfigManager{
		baseConfigManager: base_config.NewManager(configFile, config),
	}
}

// Close - to close out the channel for this object. This should only be called when the service is being shutdown
func (manager *ConfigManager) Close() {
	manager.baseConfigManager.Close()
}

// GetProjectUpdateConfig - get project update config
func (manager *ConfigManager) GetProjectUpdateConfig() ProjectUpdateConfig {
	aggregateConfig, ok := manager.baseConfigManager.Config.(aggregateConfig)
	if !ok {
		log.Error("baseConfigManager.Config is not of type 'aggregateConfig'")
		os.Exit(1)
	}
	return aggregateConfig.ProjectUpdateConfig
}

// UpdateProjectUpdateConfig - update the project update config
func (manager *ConfigManager) UpdateProjectUpdateConfig(projectUpdateConfig ProjectUpdateConfig) error {
	return manager.updateConfig(func(config aggregateConfig) (aggregateConfig, error) {
		config.ProjectUpdateConfig = projectUpdateConfig
		return config, nil
	})
}

func (manager *ConfigManager) updateConfig(updateFunc func(aggregateConfig) (aggregateConfig, error)) error {
	return manager.baseConfigManager.UpdateConfig(func(config interface{}) (interface{}, error) {
		return updateFunc(config.(aggregateConfig))
	})
}

func readConfigFromFile(configFile string, defaultConfig aggregateConfig) interface{} {
	config := defaultConfig

	if _, err := os.Stat(configFile); os.IsNotExist(err) {
		// config file does not exists
		return config
	}

	tomlData, err := ioutil.ReadFile(configFile)
	if err != nil {
		log.WithFields(log.Fields{
			"config_file": configFile,
		}).WithError(err).Error("Unable to read config file")

		return defaultConfig
	}

	err = toml.Unmarshal(tomlData, &config)
	if err != nil {
		log.WithFields(log.Fields{
			"config_file": configFile,
		}).WithError(err).Error("Unable to load manager configuration")

		return defaultConfig
	}

	return config
}
