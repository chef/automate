package config

import (
	"io/ioutil"
	"os"

	"github.com/chef/automate/api/interservice/ingest"
	toml "github.com/pelletier/go-toml"
	log "github.com/sirupsen/logrus"
	"github.com/spf13/viper"
)

// TODO @afiune We are unable to use this custom type because the underlying go-toml
// library we use to unmarshal the configuration doesn't support these cases:
// => https://github.com/pelletier/go-toml/blob/master/marshal.go#L341-L345
//
// When using this field and trying to unmarshal we get the following error:
// ```
// ingest-service.default(O): panic: reflect.Set: value of type int is not assignable to type config.job
// ```
//
// type job identifier
//type job int

// Enum jobs
const (
	DeleteNodes int = iota
	NodesMissing
	MissingNodesForDeletion
)

// List of jobs
var JobList = map[int]string{
	DeleteNodes:             "delete_nodes",
	NodesMissing:            "missing_nodes",
	MissingNodesForDeletion: "missing_nodes_for_deletion",
}

// JobConfig is the config for a job
type JobConfig struct {
	// The ID of the job
	ID int `toml:"id"`
	// The threshold time that the job will use internally to do a task
	Threshold string `toml:"threshold"`
	// How often to run the job
	Every string `toml:"every"`
	// Is the job running
	Running bool `toml:"running"`
}

// ApplyJobSettings will apply the provided settings to the job configuration
// returns 'true' if the configuration was modified to indicate that an update is needed
func (jc *JobConfig) ApplyJobSettings(settings *ingest.JobSettings) (bool, error) {
	var (
		retrigger = false
		err       = settings.Validate()
	)

	if err != nil {
		return retrigger, err
	}

	if e := settings.GetEvery(); len(e) > 0 && jc.Every != e {
		jc.Every = e
		retrigger = true
	}

	if t := settings.GetThreshold(); len(t) > 0 && jc.Threshold != t {
		jc.Threshold = t
		retrigger = true
	}

	if settings.GetRunning() != jc.Running {
		jc.Running = settings.GetRunning()
		retrigger = true
	}

	return retrigger, nil
}

// JobName is the name of the job
func (jc *JobConfig) JobName() string {
	return JobList[jc.ID]
}

// JobSchedulerConfig - the config for the job scheduler
type JobSchedulerConfig struct {
	// Is the job scheduler running
	Running bool `toml:"running"`
}

func defaultConfig() aggregateConfig {
	return aggregateConfig{
		JobSchedulerConfig: JobSchedulerConfig{
			Running: true,
		},
		JobsConfig: []JobConfig{
			DeleteNodes: JobConfig{
				ID:        DeleteNodes,
				Threshold: "1d",
				Every:     "15m",
				Running:   false,
			},
			NodesMissing: JobConfig{
				ID:        NodesMissing,
				Threshold: "1d",
				Every:     "15m",
				Running:   true,
			},
			MissingNodesForDeletion: JobConfig{
				ID:        MissingNodesForDeletion,
				Threshold: "30d",
				Every:     "15m",
				Running:   true,
			},
		},
	}
}

// Manager - configuration manager for the service
//
// This manager is multiple goroutines safe. Multiple goroutines can use one of these objects,
// and data updates are performed asynchronously. When an update function is pushed onto a channel, a
// single goroutine handles the update. When an update occurs the data is written to the config file.
// Read access is synchronous, where there is some time between seeing updates to the data.
// So if you did a config.UpdateDeleteNodesSchedulerConfig() and config.GetDeleteNodesSchedulerConfig()
// right after each other, the data that is returned from the Get would not be the data that was updated.
// This is patterned after Akka's Agents (https://doc.akka.io/docs/akka/2.5.6/java/agents.html)
type Manager struct {
	config      aggregateConfig
	updateQueue chan<- func(aggregateConfig) aggregateConfig
}

// Config - stores the configuration for the service
type aggregateConfig struct {
	JobsConfig         []JobConfig        `toml:"jobs_config"`
	JobSchedulerConfig JobSchedulerConfig `toml:"job_scheduler_config"`
}

// NewManager - create a new config. There should only be one config for the service.
func NewManager() *Manager {
	config := readinConfig()

	updateQueue := make(chan func(aggregateConfig) aggregateConfig, 100)
	manager := &Manager{
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
func (manager *Manager) Close() {
	// closes the updateQueue channel and ends that update goroutine
	close(manager.updateQueue)
}

// GetJobSchedulerConfig get the job scheduler config
func (manager *Manager) GetJobSchedulerConfig() JobSchedulerConfig {
	return manager.config.JobSchedulerConfig
}

// UpdateJobSchedulerConfig - update the job scheduler config
func (manager *Manager) UpdateJobSchedulerConfig(jobSchedulerConfig JobSchedulerConfig) error {
	errc := make(chan error)

	updateFunc := func(config aggregateConfig) aggregateConfig {
		config.JobSchedulerConfig = jobSchedulerConfig
		errc <- saveToFile(config)
		return config
	}

	manager.send(updateFunc)
	return <-errc
}

// GetJobsID returns a list of job ids loaded in the manager config
func (manager *Manager) GetJobsID() []int {
	ids := make([]int, len(manager.config.JobsConfig))
	for i, j := range manager.config.JobsConfig {
		ids[i] = j.ID
	}

	return ids
}

// GetJobConfig returns the configuration of the provided job.
// if the job doesn't exist, it returns an empty config
func (manager *Manager) GetJobConfig(jID int) JobConfig {
	for _, j := range manager.config.JobsConfig {
		if j.ID == jID {
			return j
		}
	}
	return JobConfig{}
}

// GetDeleteNodesSchedulerConfig get the delete node config
func (manager *Manager) GetDeleteNodesSchedulerConfig() JobConfig {
	return manager.GetJobConfig(DeleteNodes)
}

// GetNodesMissingSchedulerConfig get the node missing config
func (manager *Manager) GetNodesMissingSchedulerConfig() JobConfig {
	return manager.GetJobConfig(NodesMissing)
}

// GetMissingNodesForDeletionSchedulerConfig get the node missing config
func (manager *Manager) GetMissingNodesForDeletionSchedulerConfig() JobConfig {
	return manager.GetJobConfig(MissingNodesForDeletion)
}

// UpdateDeleteNodesSchedulerConfig - update the delete node config
func (manager *Manager) UpdateDeleteNodesSchedulerConfig(jConfig JobConfig) error {
	errc := make(chan error)

	updateFunc := func(config aggregateConfig) aggregateConfig {
		config.JobsConfig[DeleteNodes] = jConfig
		errc <- saveToFile(config)
		return config
	}

	manager.send(updateFunc)
	return <-errc
}

// UpdateNodesMissingSchedulerConfig update the missing nodes for deletion task
func (manager *Manager) UpdateNodesMissingSchedulerConfig(jConfig JobConfig) error {
	errc := make(chan error)

	updateFunc := func(config aggregateConfig) aggregateConfig {
		config.JobsConfig[NodesMissing] = jConfig
		errc <- saveToFile(config)
		return config
	}

	manager.send(updateFunc)
	return <-errc
}

// UpdateMissingNodesForDeletionSchedulerConfig update the missing nodes for deletion config
func (manager *Manager) UpdateMissingNodesForDeletionSchedulerConfig(jConfig JobConfig) error {
	errc := make(chan error)

	updateFunc := func(config aggregateConfig) aggregateConfig {
		config.JobsConfig[MissingNodesForDeletion] = jConfig
		errc <- saveToFile(config)
		return config
	}

	manager.send(updateFunc)
	return <-errc
}

func (manager *Manager) send(updateFunc func(aggregateConfig) aggregateConfig) {
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
