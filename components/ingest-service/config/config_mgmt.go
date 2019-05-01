package config

import (
	"io/ioutil"
	"os"

	"github.com/chef/automate/api/interservice/ingest"
	base_config "github.com/chef/automate/lib/config"
	toml "github.com/pelletier/go-toml"
	log "github.com/sirupsen/logrus"
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

const (
	RunningState    = "running"
	NotRunningState = "not_running"
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

// ProjectUpdateConfig - the config for project updating
type ProjectUpdateConfig struct {
	State           string `toml:"state"`
	ProjectUpdateID string `toml:"project_update_id"`
	EsJobID         string `toml:"es_job_id"`
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
		ProjectUpdateConfig: ProjectUpdateConfig{
			State: NotRunningState,
		},
	}
}

// Manager - configuration manager for the service
type Manager struct {
	baseConfigManager *base_config.Manager
}

// Config - stores the configuration for the service
type aggregateConfig struct {
	JobsConfig          []JobConfig         `toml:"jobs_config"`
	JobSchedulerConfig  JobSchedulerConfig  `toml:"job_scheduler_config"`
	ProjectUpdateConfig ProjectUpdateConfig `toml:"project_update_config"`
}

// NewManager - create a new config. There should only be one config for the service.
func NewManager(configFile string) *Manager {
	config := readConfigFromFile(configFile, defaultConfig())
	return &Manager{
		baseConfigManager: base_config.NewManager(configFile, config),
	}
}

// Close - close channels
func (manager *Manager) Close() {
	manager.baseConfigManager.Close()
}

// GetJobSchedulerConfig get the job scheduler config
func (manager *Manager) GetJobSchedulerConfig() JobSchedulerConfig {
	return manager.getConfig().JobSchedulerConfig
}

// UpdateJobSchedulerConfig - update the job scheduler config
func (manager *Manager) UpdateJobSchedulerConfig(jobSchedulerConfig JobSchedulerConfig) error {
	return manager.updateConfig(func(config aggregateConfig) (aggregateConfig, error) {
		config.JobSchedulerConfig = jobSchedulerConfig
		return config, nil
	})
}

// GetProjectUpdateConfig - get the project update config data
func (manager *Manager) GetProjectUpdateConfig() ProjectUpdateConfig {
	return manager.getConfig().ProjectUpdateConfig
}

// UpdateProjectUpdateConfig - update the project update config
func (manager *Manager) UpdateProjectUpdateConfig(projectUpdateConfig ProjectUpdateConfig) error {
	return manager.updateConfig(func(config aggregateConfig) (aggregateConfig, error) {
		config.ProjectUpdateConfig = projectUpdateConfig
		return config, nil
	})
}

// GetJobsID returns a list of job ids loaded in the manager config
func (manager *Manager) GetJobsID() []int {
	ids := make([]int, len(manager.getConfig().JobsConfig))
	for i, j := range manager.getConfig().JobsConfig {
		ids[i] = j.ID
	}

	return ids
}

// GetJobConfig returns the configuration of the provided job.
// if the job doesn't exist, it returns an empty config
func (manager *Manager) GetJobConfig(jID int) JobConfig {
	for _, j := range manager.getConfig().JobsConfig {
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
	return manager.updateConfig(func(config aggregateConfig) (aggregateConfig, error) {
		config.JobsConfig[DeleteNodes] = jConfig
		return config, nil
	})
}

// UpdateNodesMissingSchedulerConfig update the missing nodes for deletion task
func (manager *Manager) UpdateNodesMissingSchedulerConfig(jConfig JobConfig) error {
	return manager.updateConfig(func(config aggregateConfig) (aggregateConfig, error) {
		config.JobsConfig[NodesMissing] = jConfig
		return config, nil
	})
}

// UpdateMissingNodesForDeletionSchedulerConfig update the missing nodes for deletion config
func (manager *Manager) UpdateMissingNodesForDeletionSchedulerConfig(jConfig JobConfig) error {
	return manager.updateConfig(func(config aggregateConfig) (aggregateConfig, error) {
		config.JobsConfig[MissingNodesForDeletion] = jConfig
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

func readConfigFromFile(configFile string, defaultConfig aggregateConfig) interface{} {
	config := defaultConfig

	tomlData, err := ioutil.ReadFile(configFile)
	if os.IsNotExist(err) {
		// config file does not exists
		return config
	} else if err != nil {
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
