package config

import (
	base_config "github.com/chef/automate/lib/config"
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
	return &ConfigManager{
		baseConfigManager: base_config.NewManager(configFile, defaultConfig()),
	}
}

// Close - to close out the channel for this object. This should only be called when the service is being shutdown
func (manager *ConfigManager) Close() {
	manager.baseConfigManager.Close()
}

// GetProjectUpdateConfig
func (manager *ConfigManager) GetProjectUpdateConfig() ProjectUpdateConfig {
	return manager.baseConfigManager.Config.(aggregateConfig).ProjectUpdateConfig
}

// UpdateProjectUpdateConfig - update the project update config
func (manager *ConfigManager) UpdateProjectUpdateConfig(projectUpdateConfig ProjectUpdateConfig) error {
	errc := make(chan error)

	updateFunc := func(config aggregateConfig) aggregateConfig {
		config.ProjectUpdateConfig = projectUpdateConfig
		errc <- manager.baseConfigManager.SaveToFile(config)
		return config
	}

	manager.send(updateFunc)
	return <-errc
}

func (manager *ConfigManager) send(updateFunc func(aggregateConfig) aggregateConfig) {
	baseUpdateFunc := func(config interface{}) interface{} {
		return updateFunc(config.(aggregateConfig))
	}
	manager.baseConfigManager.Send(baseUpdateFunc)
}
