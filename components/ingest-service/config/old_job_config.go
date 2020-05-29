package config

import (
	"io/ioutil"

	"github.com/chef/automate/components/ingest-service/serveropts"
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
	Config            *Config
	DefaultJobConfigs []JobConfig
}

type Config struct {
	JobsConfig []JobConfig `toml:"jobs_config"`
}

// ConfigForJob returns the configuration for the given job index. The
// job indexes are constants in this package and reflect this
// historical structure of the on-disk job configuration. The default
// configuration is used if the configuration for a given index cannot
// be found.
func (c *OldJobConfig) ConfigForJob(jobIndex int) (JobConfig, error) {
	if c != nil && jobIndex < len(c.Config.JobsConfig) {
		return c.Config.JobsConfig[jobIndex], nil
	} else if jobIndex < len(c.DefaultJobConfigs) {
		return c.DefaultJobConfigs[jobIndex], nil
	}
	return JobConfig{}, errors.New("no default configuration for job")
}

// NewOldJobConfig - Create a OldJobConfig
func NewOldJobConfig(opts serveropts.JobsConfig) *OldJobConfig {
	return &OldJobConfig{
		Config: &Config{},
		DefaultJobConfigs: []JobConfig{
			DeleteNodes: {
				ID:        DeleteNodes,
				Threshold: "1d",
				Every:     "15m",
				Running:   false,
			},
			NodesMissing: {
				ID:        NodesMissing,
				Threshold: "1d",
				Every:     "15m",
				Running:   opts.NodesMissingRunningDefault,
			},
			MissingNodesForDeletion: {
				ID:        MissingNodesForDeletion,
				Threshold: "30d",
				Every:     "15m",
				Running:   opts.MissingNodesForDeletionRunningDefault,
			},
		},
	}
}

func (c *OldJobConfig) FromFile(configFile string) error {
	tomlData, err := ioutil.ReadFile(configFile)
	if err != nil {
		return errors.Wrap(err, "unable to read old job config")
	}

	err = toml.Unmarshal(tomlData, c.Config)
	if err != nil {
		return errors.Wrap(err, "unable to parse old job config")
	}

	return nil
}
