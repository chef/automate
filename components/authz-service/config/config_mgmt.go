package config

import (
	"io/ioutil"
	"os"
	"time"

	"github.com/chef/automate/components/automate-deployment/pkg/toml"
	base_config "github.com/chef/automate/lib/config"
	event_ids "github.com/chef/automate/lib/event"
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
				ProjectUpdateDomainService{
					Name:       event_ids.ComplianceInspecReportProducerID,
					LastUpdate: time.Now(),
				},
				ProjectUpdateDomainService{
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
func NewManager(configFile string) *Manager {
	config := readinConfig(configFile, defaultConfig())
	return &Manager{
		baseConfigManager: base_config.NewManager(configFile, config),
	}
}

// Close - to close out the channel for this object.
// This should only be called when the service is being shutdown
func (manager *Manager) Close() {
	manager.baseConfigManager.Close()
}

// GetProjectUpdateStage - get the project update stage
func (manager *Manager) GetProjectUpdateStage() ProjectUpdateStage {
	return manager.baseConfigManager.Config.(aggregateConfig).ProjectUpdateStage
}

// UpdateProjectUpdateStage - update the project update stage
func (manager *Manager) UpdateProjectUpdateStage(projectUpdateStage ProjectUpdateStage) error {
	errc := make(chan error)

	updateFunc := func(config aggregateConfig) aggregateConfig {
		config.ProjectUpdateStage = projectUpdateStage
		errc <- manager.baseConfigManager.SaveToFile(config)
		return config
	}

	manager.send(updateFunc)
	return <-errc
}

func (manager *Manager) send(updateFunc func(aggregateConfig) aggregateConfig) {
	baseUpdateFunc := func(config interface{}) interface{} {
		return updateFunc(config.(aggregateConfig))
	}
	manager.baseConfigManager.Send(baseUpdateFunc)
}

func readinConfig(configFile string, defaultConfig aggregateConfig) interface{} {
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
	}

	err = toml.Unmarshal(tomlData, &config)
	if err != nil {
		log.WithFields(log.Fields{
			"config_file": configFile,
		}).WithError(err).Error("Unable to load manager configuration")
	}

	return config
}
