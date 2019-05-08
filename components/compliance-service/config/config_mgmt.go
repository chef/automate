package config

import (
	"io/ioutil"
	"os"

	project_update_lib "github.com/chef/automate/lib/authz"
	base_config "github.com/chef/automate/lib/config"
	toml "github.com/pelletier/go-toml"
	log "github.com/sirupsen/logrus"
)

func defaultConfig() aggregateConfig {
	return aggregateConfig{
		ProjectUpdateConfig: project_update_lib.ProjectUpdateConfig{
			State: project_update_lib.NotRunningState,
		},
	}
}

// ConfigManager - configuration manager for the service
type ConfigManager struct {
	baseConfigManager *base_config.Manager
}

// Config - stores the configuration for the service
type aggregateConfig struct {
	ProjectUpdateConfig project_update_lib.ProjectUpdateConfig `toml:"project_update_config"`
}

// NewConfigManager - create a new config. There should only be one config for the service.
func NewConfigManager(configFile string) (*ConfigManager, error) {
	storedConfig, err := readConfigFromFile(configFile, defaultConfig())
	if err != nil {
		return &ConfigManager{}, err
	}

	// Testing Updating
	baseConfigManager := base_config.NewManager(configFile, storedConfig)
	err = baseConfigManager.UpdateConfig(func(config interface{}) (interface{}, error) {
		return storedConfig, nil
	})
	if err != nil {
		return &ConfigManager{}, err
	}

	return &ConfigManager{
		baseConfigManager: baseConfigManager,
	}, err
}

// Close - to close out the channel for this object. This should only be called when the service is being shutdown
func (manager *ConfigManager) Close() {
	manager.baseConfigManager.Close()
}

// GetProjectUpdateConfig - get project update config
func (manager *ConfigManager) GetProjectUpdateConfig() project_update_lib.ProjectUpdateConfig {
	aggregateConfig, ok := manager.baseConfigManager.Config.(aggregateConfig)
	if !ok {
		log.Error("baseConfigManager.Config is not of type 'aggregateConfig'")
		os.Exit(1)
	}
	return aggregateConfig.ProjectUpdateConfig
}

// UpdateProjectUpdateConfig - update the project update config
func (manager *ConfigManager) UpdateProjectUpdateConfig(projectUpdateConfig project_update_lib.ProjectUpdateConfig) error {
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

func readConfigFromFile(configFile string, defaultConfig aggregateConfig) (interface{}, error) {
	config := defaultConfig

	tomlData, err := ioutil.ReadFile(configFile)
	if os.IsNotExist(err) {
		// config file does not exists use the default config
		return config, nil
	} else if err != nil {
		log.WithFields(log.Fields{
			"config_file": configFile,
		}).WithError(err).Error("Unable to read config file")

		return defaultConfig, err
	}

	err = toml.Unmarshal(tomlData, &config)
	if err != nil {
		log.WithFields(log.Fields{
			"config_file": configFile,
		}).WithError(err).Error("Unable to load manager configuration")

		return defaultConfig, nil
	}

	return config, nil
}
