package config_test

import (
	"io/ioutil"
	"os"
	"testing"

	"github.com/chef/automate/components/ingest-service/config"
	subject "github.com/chef/automate/components/ingest-service/config"
	"github.com/stretchr/testify/assert"
)

const cFile = "/tmp/.ingest-service-test-delete-me.toml"

func TestManagerNewDefaultConfig(t *testing.T) {
	config, err := subject.NewManager(cFile)
	defer config.Close()
	assert.NoError(t, err)

	// Only three jobs configured
	assert.Equal(t, 3, len(config.GetJobsID()))

	schedulerConfig := config.GetJobSchedulerConfig()
	assert.Equal(t, true, schedulerConfig.Running, "the scheduler should be running by default")
}

func TestManagerBadFile(t *testing.T) {
	_, err := subject.NewManager("")
	assert.Error(t, err)
}

func TestManagerGetJobConfigThatExists(t *testing.T) {
	config, err := subject.NewManager(cFile)
	defer config.Close()
	assert.NoError(t, err)

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
	defer os.Remove(cFile)
	assert.Nil(t, err)

	// New config should load the file
	config, err := subject.NewManager(cFile)
	defer config.Close()
	assert.NoError(t, err)
	schedulerConfig := config.GetJobSchedulerConfig()
	assert.Equal(t, false, schedulerConfig.Running, "the scheduler should not be running")

	schedulerConfig.Running = true
	config.UpdateJobSchedulerConfig(schedulerConfig)

	assert.Equal(t, true, schedulerConfig.Running, "the scheduler should be running")
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
	defer os.Remove(cFile)
	assert.Nil(t, err)

	// When the file is corrupt, we log out the failure and use the default config
	configMgr, err := subject.NewManager(cFile)
	defer configMgr.Close()
	assert.NoError(t, err)

	schedulerConfig := configMgr.GetJobSchedulerConfig()
	assert.Equal(t, true, schedulerConfig.Running, "the scheduler should be running by default")

	projectUpdateConfig := configMgr.GetProjectUpdateConfig()
	assert.Equal(t, config.NotRunningState, projectUpdateConfig.State, "the scheduler should be running by default")
}

func TestManagerWriteConfigOnUpdatesAndLoadChanges(t *testing.T) {
	// Make sure there is no config file
	os.Remove(cFile)

	// Default config
	var (
		config, err            = subject.NewManager(cFile)
		deleteNodesConfig      = config.GetDeleteNodesSchedulerConfig()
		nodesMissingConfig     = config.GetNodesMissingSchedulerConfig()
		missingNodes4DelConfig = config.GetMissingNodesForDeletionSchedulerConfig()
	)
	defer config.Close()
	assert.NoError(t, err)

	assert.Equal(t, true, nodesMissingConfig.Running, "'nodes_missing' job should be enabled")
	assert.Equal(t, false, deleteNodesConfig.Running, "'delete_nodes' job should be disabled")
	assert.Equal(t, true, missingNodes4DelConfig.Running, "'missing_nodes_for_deletion' job should be enabled")
	assert.Equal(t, "30d", missingNodes4DelConfig.Threshold, "'missing_nodes_for_deletion' threshold should be 30d")
	// Lets update the default threshold and enable the missing_nodes_for_deletion job
	missingNodes4DelConfig.Threshold = "15d"
	missingNodes4DelConfig.Running = false
	config.UpdateMissingNodesForDeletionSchedulerConfig(missingNodes4DelConfig)
	// Check that we saved the config on disk
	_, err = os.Stat(cFile)
	assert.Nil(t, err, "config file should exist")

	// Create a new Manager config that should load the config automatically
	newConfig, err := subject.NewManager(cFile)
	defer newConfig.Close()
	assert.NoError(t, err)
	assertEqualManagerConfigs(t, newConfig, config)

	// Finally check that the object was updated
	newMissingNodes4DelConfig := config.GetMissingNodesForDeletionSchedulerConfig()
	assert.Equal(t, missingNodes4DelConfig, newMissingNodes4DelConfig, "the new config should match")
	assert.Equal(t, "15d", newMissingNodes4DelConfig.Threshold, "threshold should be set to 15d")
	assert.Equal(t, false, newMissingNodes4DelConfig.Running, "job should be disabled")

	nodesMissingConfig.Running = false
	config.UpdateNodesMissingSchedulerConfig(nodesMissingConfig)
	assert.Equal(t, false, nodesMissingConfig.Running, "'nodes_missing' job should be enabled")
}

func TestManagerWriteConfigThreadSafe(t *testing.T) {
	// default config
	config, err := subject.NewManager(cFile)
	defer config.Close()
	assert.NoError(t, err)

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
	newConfig, err := subject.NewManager(cFile)
	defer newConfig.Close()
	assert.NoError(t, err)

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

func TestManagerConfigProjectUpdateConfig(t *testing.T) {
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
	
[project_update_config]
	es_job_id = "Cmv2zbcVT4KSEz3b98IsmQ:57143"
	project_update_id = "4256e26e-92b1-4b1d-8679-44ec74b5299a"
	state = "not_running"
  `)
	err := ioutil.WriteFile(cFile, data, 0644)
	defer os.Remove(cFile)
	assert.Nil(t, err)

	// New config should load the file
	configMgr, err := subject.NewManager(cFile)
	defer configMgr.Close()
	assert.NoError(t, err)

	projectUpdateConfig := configMgr.GetProjectUpdateConfig()
	assert.Equal(t, config.NotRunningState, projectUpdateConfig.State)
	assert.Equal(t, "4256e26e-92b1-4b1d-8679-44ec74b5299a", projectUpdateConfig.ProjectUpdateID)
	assert.Equal(t, "Cmv2zbcVT4KSEz3b98IsmQ:57143", projectUpdateConfig.EsJobID)

	projectUpdateConfig.State = "running"
	err = configMgr.UpdateProjectUpdateConfig(projectUpdateConfig)
	assert.NoError(t, err)

	projectUpdateConfig = configMgr.GetProjectUpdateConfig()
	assert.Equal(t, "running", projectUpdateConfig.State)
}
