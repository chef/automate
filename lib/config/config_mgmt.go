package config

import (
	"bytes"

	"github.com/chef/automate/lib/io/fileutils"
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
	updateQueue chan<- update
}

type update struct {
	fun  func(interface{}) (interface{}, error)
	errc chan error
}

// NewManager - create a new config. There should only be one config for the service.
func NewManager(configFile string, initialConfig interface{}) *Manager {
	updateQueue := make(chan update, 100)
	manager := &Manager{
		Config:      initialConfig,
		configFile:  configFile,
		updateQueue: updateQueue,
	}

	// Single goroutine that updates the Config data and saves the data to the config file.
	go manager.configUpdater(updateQueue)

	return manager
}

// Close - to close out the channel for this object. This should only be called when the service is being shutdown
func (manager *Manager) Close() {
	// closes the updateQueue channel and ends that update goroutine
	close(manager.updateQueue)
}

// UpdateConfig - update the config
func (manager *Manager) UpdateConfig(updateFunc func(interface{}) (interface{}, error)) error {
	errc := make(chan error)
	manager.updateQueue <- update{fun: updateFunc, errc: errc}

	// Wait for the function to run
	err := <-errc
	close(errc)
	return err
}

func (manager *Manager) configUpdater(updateQueue <-chan update) {
	for update := range updateQueue {
		// Update the config object
		c, err := update.fun(manager.Config)
		if err != nil {
			update.errc <- err
			continue
		}

		manager.Config = c

		update.errc <- manager.saveToFile(c)
	}
}

func (manager *Manager) saveToFile(config interface{}) error {
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
	err = fileutils.AtomicWrite(manager.configFile, bytes.NewReader(tomlData),
		fileutils.WithAtomicWriteFileMode(0644))
	if err != nil {
		log.WithFields(log.Fields{
			"error":       err,
			"config_file": manager.configFile,
		}).Error("Error writing config file")
		return err
	}

	return err
}
