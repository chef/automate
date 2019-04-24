package config_test

import (
	"io/ioutil"
	"os"
	"testing"

	subject "github.com/chef/automate/components/ingest-service/config"
	"github.com/spf13/viper"
	"github.com/stretchr/testify/assert"
)

const cFile = "/tmp/.ingest-service.toml"

func init() {
	viper.SetConfigFile(cFile)
}

func TestManagerNewDefaultConfig(t *testing.T) {
	config := subject.NewManager("")

	// Only three jobs configured
	assert.Equal(t, 3, len(config.GetJobsID()))

	schedulerConfig := config.GetJobSchedulerConfig()
	assert.Equal(t, true, schedulerConfig.Running, "the scheduler should be running by default")
}

func TestManagerGetJobConfigThatExists(t *testing.T) {
	config := subject.NewManager("")
	jConfig := config.GetJobConfig(subject.DeleteNodes)
	assert.Equal(t, config.GetDeleteNodesSchedulerConfig(), jConfig, "should return the 'delete_nodes' job config")

	assert.Equal(t, subject.DeleteNodes, jConfig.ID, "id should match with the enum")
	assert.Equal(t, "1d", jConfig.Threshold, "threshold should be set to 1d")
	assert.Equal(t, "15m", jConfig.Every, "job should run every 15m")
	assert.Equal(t, false, jConfig.Running, "job should be disabled")
}

func TestManagerConfigFileExists(t *testing.T) {
	// Writing config file
	data := []byte(`
[job_scheduler_config]
  running = false

[[jobs_config]]
  every = "1m"
  id = 0
  running = false
  threshold = "1d"

[[jobs_config]]
  every = "1m"
  id = 1
  running = true
  threshold = "1d"

[[jobs_config]]
  every = "1m"
  id = 2
  running = true
  threshold = "1d"
  `)
	err := ioutil.WriteFile(cFile, data, 0644)
	assert.Nil(t, err)

	// New config should load the file
	config := subject.NewManager("")
	schedulerConfig := config.GetJobSchedulerConfig()
	assert.Equal(t, false, schedulerConfig.Running, "the scheduler should not be running")
}

func TestManagerConfigFileExistsButItsCorrupt(t *testing.T) {
	// Writing config file
	data := []byte(`
[job_scheduler_config]
  running = false

every = "1m"
id = 0
running = false
threshold = "1d"

[[jobs_config]]
  every = "1m"
  id = 2
  running = true
  threshold = "1d"
  `)
	err := ioutil.WriteFile(cFile, data, 0644)
	assert.Nil(t, err)

	// When the file is corrupt, we log out the failure and use the default config
	config := subject.NewManager("")
	schedulerConfig := config.GetJobSchedulerConfig()
	assert.Equal(t, true, schedulerConfig.Running, "the scheduler should be running by default")
}

func TestManagerWriteConfigOnUpdatesAndLoadChanges(t *testing.T) {
	// Make sure there is no config file
	os.Remove(cFile)

	// Default config
	var (
		config                 = subject.NewManager("")
		deleteNodesConfig      = config.GetDeleteNodesSchedulerConfig()
		nodesMissingConfig     = config.GetNodesMissingSchedulerConfig()
		missingNodes4DelConfig = config.GetMissingNodesForDeletionSchedulerConfig()
	)

	assert.Equal(t, true, nodesMissingConfig.Running, "'nodes_missing' job should be enabled")
	assert.Equal(t, false, deleteNodesConfig.Running, "'delete_nodes' job should be disabled")
	assert.Equal(t, true, missingNodes4DelConfig.Running, "'missing_nodes_for_deletion' job should be enabled")
	assert.Equal(t, "30d", missingNodes4DelConfig.Threshold, "'missing_nodes_for_deletion' threshold should be 30d")
	// Lets update the default threshold and enable the missing_nodes_for_deletion job
	missingNodes4DelConfig.Threshold = "15d"
	missingNodes4DelConfig.Running = false
	config.UpdateMissingNodesForDeletionSchedulerConfig(missingNodes4DelConfig)
	// Check that we saved the config on disk
	_, err := os.Stat(cFile)
	assert.Nil(t, err, "config file should exist")

	// Create a new Manager config that should load the config automatically
	newConfig := subject.NewManager("")
	assertEqualManagerConfigs(t, newConfig, config)

	// Finally check that the object was updated
	newMissingNodes4DelConfig := config.GetMissingNodesForDeletionSchedulerConfig()
	assert.Equal(t, missingNodes4DelConfig, newMissingNodes4DelConfig, "the new config should match")
	assert.Equal(t, "15d", newMissingNodes4DelConfig.Threshold, "threshold should be set to 15d")
	assert.Equal(t, false, newMissingNodes4DelConfig.Running, "job should be disabled")
}

func TestManagerWriteConfigThreadSafe(t *testing.T) {
	// default config
	config := subject.NewManager("")

	// Lets use this job to send multiple updates
	jConfig := config.GetDeleteNodesSchedulerConfig()
	// Send 500 updates
	for i := 0; i < 500; i++ {
		// turn job on and off
		if jConfig.Running {
			jConfig.Running = false
		} else {
			jConfig.Running = true
		}
		config.UpdateDeleteNodesSchedulerConfig(jConfig)
	}

	// After smashing the config with updates
	newJobConfig := config.GetDeleteNodesSchedulerConfig()
	assert.Equal(t, jConfig, newJobConfig, "jobs should match")

	// Create a new Manager config that should load the config automatically
	newConfig := subject.NewManager("")
	assertEqualManagerConfigs(t, newConfig, config)
}

// assertEqualManagerConfigs verifies that two manager configs are equal
func assertEqualManagerConfigs(t *testing.T, c1, c2 *subject.Manager) {
	for job := range c1.GetJobsID() {
		j1 := c1.GetJobConfig(job)
		j2 := c2.GetJobConfig(job)
		assert.Equal(t, j1, j2, "jobs should match")
	}
}
