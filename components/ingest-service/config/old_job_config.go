package config

import (
	"io/ioutil"

	toml "github.com/pelletier/go-toml"
	"github.com/pkg/errors"
)

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

type OldJobConfig struct {
	JobsConfig []JobConfig `toml:"jobs_config"`
}

// ConfigForJob returns the configuration for the given job index. The
// job indexes are constants in this package and reflect this
// historical structure of the on-disk job configuration. The default
// configuration is used if the configuration for a given index cannot
// be found.
func (c *OldJobConfig) ConfigForJob(jobIndex int) (JobConfig, error) {
	if c != nil && jobIndex < len(c.JobsConfig) {
		return c.JobsConfig[jobIndex], nil
	} else if jobIndex < len(defaultJobConfig) {
		return defaultJobConfig[jobIndex], nil
	}
	return JobConfig{}, errors.New("no default configuration for job")
}

var defaultJobConfig = []JobConfig{
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
}

func OldJobConfigFromFile(configFile string) (*OldJobConfig, error) {
	config := &OldJobConfig{}
	tomlData, err := ioutil.ReadFile(configFile)
	if err != nil {
		return nil, errors.Wrap(err, "unable to read old job config")
	}

	err = toml.Unmarshal(tomlData, config)
	if err != nil {
		return nil, errors.Wrap(err, "unable to parse old job config")
	}

	return config, nil
}
