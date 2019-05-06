package config

import (
	"io/ioutil"
	"os"
	"time"

	base_config "github.com/chef/automate/lib/config"
	event_ids "github.com/chef/automate/lib/event"
	toml "github.com/pelletier/go-toml"
	log "github.com/sirupsen/logrus"
)

const (
	RunningState    = "running"
	NotRunningState = "not_running"
)

func defaultConfig() aggregateConfig {
	return aggregateConfig{
		ProjectUpdateStage: ProjectUpdateStage{
			State: NotRunningState,
			DomainServices: []ProjectUpdateDomainService{
				{
					Name:       event_ids.ComplianceInspecReportProducerID,
					LastUpdate: time.Now(),
				},
				{
					Name:       event_ids.InfraClientRunsProducerID,
					LastUpdate: time.Now(),
				},
			},
		},
	}
}

// Manager - configuration manager for the service
type Manager struct {
	baseConfigManager *base_config.Manager
}

// Config - stores the configuration for the service
type aggregateConfig struct {
	ProjectUpdateStage ProjectUpdateStage `toml:"project_update_config"`
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

// Close - to close out the channel for this object.
// This should only be called when the service is being shutdown
func (manager *Manager) Close() {
	manager.baseConfigManager.Close()
}

// GetProjectUpdateStage - get the project update stage
func (manager *Manager) GetProjectUpdateStage() ProjectUpdateStage {
	aggregateConfig, ok := manager.baseConfigManager.Config.(aggregateConfig)
	if !ok {
		log.Error("baseConfigManager.Config is not of type 'aggregateConfig'")
		os.Exit(1)
	}
	return aggregateConfig.ProjectUpdateStage
}

// UpdateProjectUpdateStage - update the project update stage
func (manager *Manager) UpdateProjectUpdateStage(projectUpdateStage ProjectUpdateStage) error {
	return manager.updateConfig(func(config aggregateConfig) (aggregateConfig, error) {
		config.ProjectUpdateStage = projectUpdateStage
		return config, nil
	})
}

func (manager *Manager) updateConfig(updateFunc func(aggregateConfig) (aggregateConfig, error)) error {
	return manager.baseConfigManager.UpdateConfig(func(config interface{}) (interface{}, error) {
		return updateFunc(config.(aggregateConfig))
	})
}

// If there is an error in reading the file return the default file.
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
