package config

import (
	"io/ioutil"
	"os"

	toml "github.com/pelletier/go-toml"
	log "github.com/sirupsen/logrus"
)

// Manager - configuration manager for the service
//
// This manager is multiple goroutines safe. Multiple goroutines can use one of these objects,
// and data updates are performed asynchronously. When an update function is pushed onto a channel, a
// single goroutine handles the update. When an update occurs the data is written to the config file.
// Read access is synchronous, where there is some time between seeing updates to the data.
// This is patterned after Akka's Agents (https://doc.akka.io/docs/akka/2.5.6/java/agents.html)
type Manager struct {
	Config      interface{}
	configFile  string
	updateQueue chan<- func(interface{}) interface{}
}

// NewManager - create a new config. There should only be one config for the service.
func NewManager(configFile string, initialConfig interface{}) *Manager {
	updateQueue := make(chan func(interface{}) interface{}, 100)
	manager := &Manager{
		Config:      initialConfig,
		configFile:  configFile,
		updateQueue: updateQueue,
	}

	// Single goroutine that updates the Config data and saves the data to the config file.
	go func() {
		for update := range updateQueue {
			// Update the config object
			c := update(manager.Config)

			// Once the config object is updated and save to the file,
			// Update the manager's config object so 'Get' requests will
			// see the change.
			manager.Config = c
		}
	}()

	return manager
}

// Close - to close out the channel for this object. This should only be called when the service is being shutdown
func (manager *Manager) Close() {
	// closes the updateQueue channel and ends that update goroutine
	close(manager.updateQueue)
}

func (manager *Manager) Send(updateFunc func(interface{}) interface{}) {
	manager.updateQueue <- updateFunc
}

func (manager *Manager) SaveToFile(config interface{}) error {
	log.WithFields(log.Fields{
		"config_file": manager.configFile,
	}).Debug("Saving Config File")

	tomlData, err := toml.Marshal(config)
	if err != nil {
		log.WithFields(log.Fields{
			"error": err,
		}).Error("Error Marshaling Config struct")
		return err
	}

	// create a file with the permissions of the running user can read/write other can only read.
	var permissions os.FileMode = 0644
	err = ioutil.WriteFile(manager.configFile, tomlData, permissions)
	if err != nil {
		log.WithFields(log.Fields{
			"error":       err,
			"config_file": manager.configFile,
		}).Error("Error writing config file")
		return err
	}

	return err
}
