package config

import (
	"io/ioutil"
	"os"

	toml "github.com/pelletier/go-toml"
	log "github.com/sirupsen/logrus"
	"github.com/spf13/viper"
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
//
// This manager is multiple goroutines safe. Multiple goroutines can use one of these objects,
// and data updates are performed asynchronously. When an update function is pushed onto a channel, a
// single goroutine handles the update. When an update occurs the data is written to the config file.
// Read access is synchronous, where there is some time between seeing updates to the data.
// This is patterned after Akka's Agents (https://doc.akka.io/docs/akka/2.5.6/java/agents.html)
type ConfigManager struct {
	config      aggregateConfig
	updateQueue chan<- func(aggregateConfig) aggregateConfig
}

// Config - stores the configuration for the service
type aggregateConfig struct {
	ProjectUpdateConfig ProjectUpdateConfig `toml:"project_update_config"`
}

// NewConfigManager - create a new config. There should only be one config for the service.
func NewConfigManager() *ConfigManager {
	config := readinConfig()

	updateQueue := make(chan func(aggregateConfig) aggregateConfig, 100)
	manager := &ConfigManager{
		config:      config,
		updateQueue: updateQueue,
	}

	// Single goroutine that updates the Config data and saves the data to the config file.
	go func() {
		for update := range updateQueue {
			// Update the config object
			c := update(manager.config)

			// Once the config object is updated and save to the file,
			// Update the manager's config object so 'Get' requests will
			// see the change.
			manager.config = c
		}
	}()

	return manager
}

// Close - to close out the channel for this object. This should only be called when the service is being shutdown
func (manager *ConfigManager) Close() {
	// closes the updateQueue channel and ends that update goroutine
	close(manager.updateQueue)
}

// GetProjectUpdateConfig
func (manager *ConfigManager) GetProjectUpdateConfig() ProjectUpdateConfig {
	return manager.config.ProjectUpdateConfig
}

// UpdateProjectUpdateConfig - update the project update config
func (manager *ConfigManager) UpdateProjectUpdateConfig(projectUpdateConfig ProjectUpdateConfig) error {
	errc := make(chan error)

	updateFunc := func(config aggregateConfig) aggregateConfig {
		config.ProjectUpdateConfig = projectUpdateConfig
		errc <- saveToFile(config)
		return config
	}

	manager.send(updateFunc)
	return <-errc
}

func (manager *ConfigManager) send(updateFunc func(aggregateConfig) aggregateConfig) {
	manager.updateQueue <- updateFunc
}

func saveToFile(config aggregateConfig) error {

	configFile := viper.ConfigFileUsed()

	log.WithFields(log.Fields{
		"config_file": configFile,
	}).Info("Saving Config File")

	tomlData, err := toml.Marshal(config)
	if err != nil {
		log.WithFields(log.Fields{
			"error": err,
		}).Error("Error Marshaling Config struct")
		return err
	}

	// create a file with the permissions of the running user can read/write other can only read.
	var permissions os.FileMode = 0644
	err = ioutil.WriteFile(configFile, tomlData, permissions)
	if err != nil {
		log.WithFields(log.Fields{
			"error":       err,
			"config_file": configFile,
		}).Error("Error writing config file")
		return err
	}

	return err
}

func readinConfig() aggregateConfig {
	var (
		config     = defaultConfig()
		configFile = viper.ConfigFileUsed()
	)
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
