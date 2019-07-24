package config

import (
	"io/ioutil"
	"os"

	toml "github.com/pelletier/go-toml"
	log "github.com/sirupsen/logrus"

	project_update_lib "github.com/chef/automate/lib/authz"
	base_config "github.com/chef/automate/lib/config"
)

func defaultConfig() aggregateConfig {
	return aggregateConfig{
		ProjectUpdateConfig: project_update_lib.ProjectUpdateConfig{
			State: project_update_lib.NotRunningState,
		},
	}
}

// Manager - configuration manager for the service
type Manager struct {
	baseConfigManager *base_config.Manager
}

// Config - stores the configuration for the service
type aggregateConfig struct {
	ProjectUpdateConfig project_update_lib.ProjectUpdateConfig `toml:"project_update_config"`
}

// NewManager - create a new config. There should only be one config for the service.
func NewManager(configFile string) (*Manager, error) {
	storedConfig, err := readConfigFromFile(configFile, defaultConfig())
	if err != nil {
		return &Manager{}, err
	}

	// Testing Updating
	baseManager := base_config.NewManager(configFile, storedConfig)
	err = baseManager.UpdateConfig(func(config interface{}) (interface{}, error) {
		return storedConfig, nil
	})
	if err != nil {
		return &Manager{}, err
	}

	return &Manager{
		baseConfigManager: baseManager,
	}, err
}

// Close - close channels
func (manager *Manager) Close() {
	manager.baseConfigManager.Close()
}

// GetProjectUpdateConfig - get the project update config data
func (manager *Manager) GetProjectUpdateConfig() project_update_lib.ProjectUpdateConfig {
	return manager.getConfig().ProjectUpdateConfig
}

// UpdateProjectUpdateConfig - update the project update config
func (manager *Manager) UpdateProjectUpdateConfig(projectUpdateConfig project_update_lib.ProjectUpdateConfig) error {
	return manager.updateConfig(func(config aggregateConfig) (aggregateConfig, error) {
		config.ProjectUpdateConfig = projectUpdateConfig
		return config, nil
	})
}

func (manager *Manager) updateConfig(updateFunc func(aggregateConfig) (aggregateConfig, error)) error {
	return manager.baseConfigManager.UpdateConfig(func(config interface{}) (interface{}, error) {
		return updateFunc(config.(aggregateConfig))
	})
}

func (manager *Manager) getConfig() aggregateConfig {
	aggregateConfig, ok := manager.baseConfigManager.Config.(aggregateConfig)
	if !ok {
		log.Error("baseConfigManager.Config is not of type 'aggregateConfig'")
		os.Exit(1)
	}

	return aggregateConfig
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

		// Could not load data from config file using the default config.
		return defaultConfig, nil
	}

	return config, nil
}
